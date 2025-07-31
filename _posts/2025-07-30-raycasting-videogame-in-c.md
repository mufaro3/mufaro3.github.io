---
layout: post
title:  "N00M: My Attempt at Raycasting and 3-D Video Game Design in C"
date:   2025-07-30 08:44:42
comments: true
categories:
    - programming
tags:
    - c
    - in-progress
---

A few months ago, I asked myself what it would take to make a raycaster in C. I've seen other programmers before make successful raycasters, namely [jdh](https://www.youtube.com/@jdh/videos) in one of my favorite videos of his: *Programming a first person shooter from scratch like it's 1995*

{% include video_link.html url="https://www.youtube.com/embed/fSjc8vLMg8c" %}

In particular, I'm incredibly inexperienced in basically all facets of game development, but especially with 3-D games. I've only ever made a singular useful graphical program before, [mufaro3/sorting-visualization](https://github.com/mufaro3/sorting-visualization), and it was in C++ using SFML rather than C, and the most I had attempted graphical program design in C was using some basic SDL to draw a few shapes onto the screen about a year or so prior. The bottom line is that in starting this project, I might as well be jumping off into the void.

However, I was really certain that I wanted to understand the basics of 3-D software rendering, as it seemed so incredibly complicated. Therefore, what I set out to do in [mufaro3/n00m](https://github.com/mufaro3/n00m) was to follow [Lode's Computer Graphics Tutorial](https://lodev.org/cgtutor/raycasting.html) on raycasting engine development as much as possible to successfully build my own DOOM clone (thus, why I called it N00M, as it's my own "nickelulzian doomstyle renderer").

I opted to make N00M in C using SDL for graphics, [recp/cglm](https://github.com/recp/cglm) to simplify the linear algebra calculations, and [rxi/log.c](https://github.com/rxi/log.c) for logging. Therefore, to begin with, I set up my C directory as follows (my usual structure):

```
n00m/
├── src/
│   ├── main.c
│   └── common.h
├── lib/
│   ├── cglm
│   └── log.c
├── res
├── CMakeLists.txt
├── Makefile
├── .gitignore
├── LICENSE
├── AUTHORS
└── README
```

I first just made sure to set up my build system with a basic headerfile to test if everything could link and build successfully:

{% highlight c %}
#include "common.h"

int 
main(void)
{
  printf( "Hello, World!\n" );
  return 0;  
}
{% endhighlight %}

Once this worked, I got started on the actual program structure. Good graphical programs, even in C, are highly modularized, so I started breaking down the system into modules (essentially a variable/struct storing the state of that module for the program at a given time and a header and associated source file for that module with related functions): 

1. `graphics` for managing everything rendering/graphics related
2. `state` for the actual logical game state
3. `config` for configuration values managed under `config.h` and `config.c`

Then, I started writing out the graphics module directly. I started with some functions to initialize SDL and close the graphics module as
{% highlight c %}
void
graphics_init (graphics_t *gfx, config_t *config)
{
  if (SDL_Init(SDL_INIT_VIDEO) < 0) {
    log_fatal("SDL could not be initialized!\n"
              "SDL Error: %s", SDL_GetError());
    exit(EXIT_FAILURE);
  }

  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "1");

  int image_flags = IMG_INIT_PNG | IMG_INIT_JPG | IMG_INIT_TIF;
  int image_init  = IMG_Init(image_flags); 
  
  if ((image_init & image_flags) != image_flags) {
    log_fatal("SDL_image could not be initialized!\n"
              "SDL_image Error: %s", IMG_GetError());
    exit(EXIT_FAILURE);
  }

  if (TTF_Init() == -1) {
    log_error("SDL_TTF could not be initialized!\n"
              "SDL_TTF Error: %s", TTF_GetError());
  }
  
#if defined linux && SDL_VERSION_ATLEAST(2, 0, 8)
  /* Disable compositor bypass */
  if (!SDL_SetHint(SDL_HINT_VIDEO_X11_NET_WM_BYPASS_COMPOSITOR, "0")) {
    log_fatal("SDL cannot disable compositor bypass!");
    exit(EXIT_FAILURE);
  }
#endif

  gfx->window = SDL_CreateWindow(config->window_title,
                                 SDL_WINDOWPOS_CENTERED,
                                 SDL_WINDOWPOS_CENTERED,
                                 config->window_width,
                                 config->window_height,
                                 SDL_WINDOW_SHOWN);

  if (!gfx->window) {
    log_fatal("Window could not be created!\n"
              "SDL Error: %s", SDL_GetError());
    SDL_Quit();
    exit(EXIT_FAILURE);
  }

  gfx->renderer = SDL_CreateRenderer(gfx->window, -1,
                                     SDL_RENDERER_PRESENTVSYNC);

  if (!gfx->renderer) {
    log_fatal("Renderer could not be created!\n"
              "SDL Error: %s", SDL_GetError());
    SDL_DestroyWindow(gfx->window);
    SDL_Quit();
    exit(EXIT_FAILURE);
  }

  gfx->screen_texture = SDL_CreateTexture(gfx->renderer,
                                          SDL_PIXELFORMAT_RGBA8888,
                                          SDL_TEXTUREACCESS_STREAMING,
                                          config->window_width,
                                          config->window_height);
}

void
graphics_close (graphics_t *gfx)
{
  SDL_DestroyTexture(gfx->screen_texture);
  SDL_DestroyRenderer(gfx->renderer);
  SDL_DestroyWindow(gfx->window);

  TTF_Quit();
  IMG_Quit();
  SDL_Quit();
}
{% endhighlight %} 

The idea behind this is that every module will have this same kind of structure, an initialization function and a deinitialization (or close) function. Why this is useful is that it simplifies the general structure of my program. To see this, we can look directly at the main function:

{% highlight c %}
int
main (void)
{
  log_info("Initializing");

  /* setup the random number generator */
  srand(time(0));
  
  config_t config;
  config_load(&config);

  graphics_t gfx;
  graphics_init(&gfx, &config);

  state_t state;
  state_init(&state, &config);

  log_info("Looping");
  
  while (state.running)
    loop(&state, &gfx);
  
  log_info("Closing");
  
  graphics_close(&gfx);
  config_close(&config);
}
{% endhighlight %}

The layout of each module's data structure was pretty simple. In `graphics_t`, I included some simple SDL pointers necessary for basic rendering. The idea behind `graphics_t`, after all, is as a singular struct that simplifies passing-in the graphical state of the program into various functions (so any function with a graphical component that may need to access just the `SDL_Window` context or the `SDL_Renderer` or etc. can just simply have the entire graphical state passed in at once for simplicity).
 
{% highlight C %}
typedef struct _graphics {
  SDL_Window *window;
  SDL_Renderer *renderer;
  SDL_Texture *screen_texture;
} graphics_t;
{% endhighlight %}

Similarly, the other modules were much the same. The entire game state in `state_t` was:

{% highlight c %}
typedef struct _fpstimer {
  uint64_t ticks, frames, last_update_time;
  float tps, fps;
  bool infinite_fps;
} fpstimer_t;

/* fonts */
enum { DEJAVU_SANS_MONO, TOTAL_FONTS };

#define TEXTURE_WIDTH  64
#define TEXTURE_HEIGHT 64
#define NUM_TEXTURES   8

typedef struct _state {
  bool running;
  vec2s camera_plane;
  ivec2s map_size;
  ivec2s resolution;

  /* textures */
  uint32_t *textures[NUM_TEXTURES];
  
  /* loaded fonts */
  TTF_Font *fonts[TOTAL_FONTS];
  TTF_Font *debug_font;
  int debug_line_y;

  /* map information */
  map_t map;

  /* controls and whatnot */
  bool keys[SDL_NUM_SCANCODES];
  ivec2s mouse_pos_delta;
  float mouse_sensitivity;
  
  player_t player;
  fpstimer_t timer;
} state_t;
{% endhighlight %}

with some code for player information, the presently loaded fonts, debugging info, and some controls and timing variables. Lastly, `config_t` was much the same:

{% highlight c %}
typedef struct _config {
  uint32_t window_height, window_width;
  uint8_t debug_font_size;
  char window_title[512];

  /* system specification */
  bool is_big_endian;
} config_t;
{% endhighlight %}

Altogether, it's a fairly simple system, and from there, I sought to actually being on rendering. I started by writing in some simple graphics functions (which I won't show the implementations of, only the declarations):

{% highlight c %}
void graphics_init (graphics_t *gfx, config_t *config);
void graphics_close (graphics_t *gfx);

void graphics_draw_pixel_buffer(graphics_t *gfx, ivec2s resolution, uint32_t *pixels);
void graphics_draw_text (graphics_t *gfx, TTF_Font *font, const char *text,
			 SDL_Color text_color, SDL_Color background_color,
			 int x, int y, int padding);
{% endhighlight %}

and a map management system as well:

{% highlight c %}
/* common defines */
#define MAP_OPEN 0
#define NOT_FOUND -1

typedef struct _map {
  int *cells;
  ivec2s size;
} map_t;

SDL_Color map_get_color (int map_value);

void map_init  (map_t *map);
void map_close (map_t *map);

int map_at_vec  (map_t *map, ivec2s pos);
int map_at_fvec (map_t *map, vec2s pos);
int map_at      (map_t *map, int x, int y);

{% endhighlight %}

Finally, with this background, I could get started on the following static `main` file functions:

{% highlight c %}
static void draw_current_frame (graphics_t *gfx, state_t *state);
static void draw_mini_map (graphics_t *gfx, state_t *state);
static void poll_events (state_t *state);
static void handle_keys (state_t *state);
static void draw_debug (graphics_t *gfx, state_t *state, config_t *config);
static void loop (state_t *state, graphics_t *gfx, config_t *config);
{% endhighlight %}

The idea behind these is that each handles a specific function. The main loop of the program is performed by `loop`, events and input are handled by `poll_events` and `handle_keys`, and then we have the three main drawing functions: `draw_current_frame` for the actual game scene, `draw_mini_map` for the minimap, and `draw_debug` for the debug screen. In this post, I won't go into `draw_debug`, and I'll mostly focus on the other two.

To begin, drawing the minimap was quite simple. Essentially, what I'm doing is just first performing some calculations to determine how big the minimap should be and where it should be, then I draw the background of the minimap, and then I go in and loop through each of the squares of the minimap to draw in each square unit area by its associated color (as given by its mapvalue). 

{% highlight c %}
static void
draw_mini_map (graphics_t *gfx, state_t *state)
{
  /* minimap background constants */
  static const uint8_t BOX_SIZE    = 5; /* width of each box */
  static const uint8_t PADDING     = 1; /* padding for the background */
  static const uint8_t MARGIN      = 2; /* the outer margin */
  static const uint8_t PLAYER_SIZE = 4; /* width/height of the player icon */ 

  /* set up the box sizing for the outer box */
  ivec2s full_minimap_size = ...

  /* determine placement based on the sizing */
  ivec2s full_minimap_position = ...
  /* draw the background */
  SDL_Rect bg_rect = ...
  
  SDL_SetRenderDrawColor(gfx->renderer, EXPAND_COLOR(COLOR_WHITE));
  SDL_RenderFillRect(gfx->renderer, &bg_rect);

  ivec2s box_top_left = glms_ivec2_adds(full_minimap_position, PADDING);

  ...
  
  /* draw all of the boxes */
  for (int x = 0; x < state->map.size.x; ++x) {
    for (int y = 0; y < state->map.size.y; ++y) {      
      /* get the fill color from the position */
      int map_value = map_at(&state->map, x, y);
      SDL_Color fill_color = map_value == MAP_OPEN ? COLOR_BLACK : map_get_color(map_value);

      /* calculate the fill position from the r,c value */
      SDL_Rect box = ...
      
      /* now draw it to the screen */
      SDL_SetRenderDrawColor(gfx->renderer, EXPAND_COLOR(fill_color));
      SDL_RenderFillRect(gfx->renderer, &box);
    }
  }
  
  /* draw a tiny square to represent the player */
  ivec2s player_indicator_center = ...
  
  SDL_Rect player_indicator_bounds = ...

  ...
  
  SDL_SetRenderDrawColor(gfx->renderer, EXPAND_COLOR(COLOR_LIGHT_GRAY));
  SDL_RenderFillRect(gfx->renderer, &player_indicator_bounds);

  /* draw a line to represent the view direction */
  static const int view_line_distance = 20;

  /* normalize the direction vector then scale by the distance */
  vec2s direction_vector;
  glm_vec2_normalize_to((float*) state->player.dir.raw, direction_vector.raw);
  direction_vector = glms_vec2_scale(direction_vector, view_line_distance);

  ivec2s direction_vector_end = glms_ivec2_add(VEC2_INT(direction_vector),
                                               player_indicator_center);
  
  /* draw the vector to the screen */
  SDL_SetRenderDrawColor(gfx->renderer, EXPAND_COLOR(COLOR_LIGHT_GRAY));
  SDL_RenderDrawLine(gfx->renderer, ...
}
{% endhighlight %}

From there, I started working on the actual raycasting (the bread and butter of this program).

{% include unfinished.md %}