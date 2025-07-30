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

{% include unfinished.md %}