---
layout: post
title:  "My Setup: Tools, Technology, Editors, etc."
date:   2025-07-25 07:46:46
comments: true
categories:
    - programming
tags:
    - linux
---

On this post, I'd like to outline all of the tools, technology, and whatnot that I regularly use, perhaps it could help someone!

### Operating System, Desktop Environment, etc.

![Neofetch]({{ 'images/posts/neofetch.jpg' | relative_url }})

*The Neofetch of my desktop computer.*

I've been using Linux since I was 14 years old, but I only switched to using Linux as a daily driver on my laptop at 15 and then completely on all my devices (like my main desktop computer) at 16 or 17. I've used various distributions before, including Manjaro, Ubuntu, LinuxMint, Void, openSUSE, Fedora, and more, but Debian has always been my go-to simply because it's so reliable and stable. It's been my favorite, without a doubt, throughout the years.

![The Laptop in Question]({{ 'images/camera/DSC03399.JPG' | relative_url }})

*This is my Laptop, an old ThinkPad T480s that is also running Debian.*

I have two main computers, a desktop and a laptop, and both of these devices run Debian 12 (bookworm). My desktop has both MATE and LXQt installed (I'm currently using LXQt in the above screenshot) and my laptop has Gnome and bspwm installed. In the past, I've used other major desktop environments like XFCE and Cinnamon (I've never really liked environments like KDE), but for stacking window managers, Gnome and MATE have always felt the most natural. 

### Editors

I've used a number of text editors/IDEs over the years, originally starting out with IntelliJ Idea (which I got fairly good with after about a year or so of using) as I only really wrote Java code for assignments in high school between 2020 and 2022. As I got into competitive programming, I needed a faster editor, so I switched to Visual Studio Code, but I didn't like how restrictive the editor felt when it came to scripting and out-of-the-box shortcuts, so after about a year, I made the switch to Neovim, which I used for about two years during my senior year of high school and most of my freshman year of university.

My full Neovim config is as follows:

{% highlight lua %}
-- preplugin config

vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- text/indent config
vim.o.expandtab = true
vim.o.smartindent = true
vim.o.tabstop = 2
vim.o.shiftwidth = 2

-- nerd font
vim.g.have_nerd_font = false

-- folding
vim.o.foldmethod = 'indent'
vim.o.foldlevel = 2
vim.o.foldlevelstart = 0
vim.o.foldnestmax = 2

-- plugins

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then                                                     
  vim.fn.system({                                                                                      
    "git",                                                                                             
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end

vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  -- TreeSitter
  {
    'nvim-treesitter/nvim-treesitter',
    build = ':TSUpdate',
    opts = {
      ensure_installed = { 'bash', 'c', 'html', 'lua', 'luadoc', 'markdown', 'vim', 'vimdoc' },
      auto_install = true,
      highlight = {
        enable = true,
              additional_vim_regex_highlighting = { 'ruby' }
      },
      indent = { enable = true, disable = { 'ruby' } }
    }
  },

  -- Melange Theme
  { 'savq/melange-nvim' },

  -- Indent Blankline
  { "lukas-reineke/indent-blankline.nvim", main = "ibl", opts = {} },

  {
    'echasnovski/mini.nvim',
    config = function()
      local statusline = require 'mini.statusline'
      statusline.setup { use_icons = vim.g.have_nerd_font }
    end
  },

  {
    "lervag/vimtex",
    lazy = false,     -- we don't want to lazy load VimTeX
    -- tag = "v2.15", -- uncomment to pin to a specific release
    init = function()
      -- VimTeX configuration goes here, e.g.
      vim.g.vimtex_view_method = "general"
    end
  } 
})

vim.opt.termguicolors = true
vim.cmd.colorscheme 'melange'
{% endhighlight %}

or, at least, this was the last Neovim config I used (well over six months ago). In university, we were required to use the editor DrRacket for my *How to Design Programs* course, and when I was done with the class, I still wanted to write racket-like code (in Common Lisp, now), and although Neovim has some tools that can be used for writing lisp-family code like Vlime, I opted to just going along with the recommendations of the Lisp community and replaced DrRacket with GNU Emacs, which I now use for practically everything.

### Note-taking

When I say everything, I mean everything. Emacs is an entire environment as opposed to a text editor. If I really wanted to, I could likely remove the desktop environment of my system entirely and just run a text server and perform all of my computer functions through Emacs (although, that's insane, so I won't do it). However, there are many things that I've done with text editors that many people think is crazy. For example, I took all of my notes for the past two years, even in my Senior year of high school, entirely in LaTeX using VimTeX, meaning I still have PDF access to notes from the following classes I've taken:

1. AP Calculus BC
2. AP Physics C: Mechanics
3. AP Physics C: Electricity and Magnetism
4. AP Physics 1: Algebra-Based
   - (I actually went back and LaTeX'd up my notes from Junior year in preparation for Physics C)
5. CHEM 1405: Introductory Chemistry
6. Science One Physics
7. Science One Biology
8. Science One Chemistry
9. Science One Mathematics 
   - (essentially Differential and Integral Calculus)

I also have some notes from work I've done on my own outside of classes, like

1. Online shona tutoring,
2. Exploring Linear Algebra by Arangala, and
3. CLP Multivariable Calculus.

A minimal example of what I might write as notes in LaTeX looks like the following:

{% highlight tex %}
\usepackage{amsmath, amssymb, physics}
\usepackage{tgpagella, multicol, enumitem}

\begin{document}
Today I will prove that P = NP!

\begin{proof}
P must equal NP as
\begin{equation*}
\div \cross \va{E} = -\pdv{\va{B}}{t}
\end{equation*}
\end{proof}
\end{document}
{% endhighlight %}

and I found that with enough macros, the writing time for information becomes incredibly fast, allowing me to write notes really quickly in class (and get everything the professor is saying), a technique I learned from [Giles Castel](https://castel.dev/post/lecture-notes-1/) (rest in peace, legend) and [SeniorMars](https://youtu.be/DOtM1mrWjUo?si=PiRADPib3Z_mTNxI). 

The main drawback of this, though, is that you spend way too much time just writing down information rather than actually listening to what the professor is saying. I recommend using this for self-studying with textbooks or videos (which is what I usually do) but not for live lectures. For a live lecture, just bring a notebook and a pencil.

### My Phone, Headphones, and Keyboard

![My Phone]({{ 'images/camera/DSC03406.JPG' | relative_url }})

*This photo is a bit blurry, but it shows off my phone fairly well.*

Next on the chopping block is my peripherals. I use a TCL Flip Pro as my phone, which is basically just an mp3 player that also doubles as being able to browse the internet (so it can read RSS) and make calls. It was something like 30 dollars off eBay, and it wasn't a bad investment. 

![My Full Desk Setup]({{ 'images/camera/DSC03396.JPG' | relative_url }})

*You can see my full desk setup in this image, including my keyboard!*

For my keyboard, I use a Glorious GMMK 3 Prebuilt Keyboard with Cherry MX Brown tactile switches, alongside some custom gray-themed keycaps from Amazon. I got it when I was 15 or so, and it has held up well over the years (alongside fairly regular cleaning and lubing). It has the ability to use RGB lighting with their built-in software, but I set it to just use white backlighting something like 3 or so years ago (back when I still had windows) and then never changed it since then.

My headphones are the SkullCandy Hesh ANC, which is pretty good. I don't regularly use it at my computer anymore, not like I used to, instead opting for some cheap wired earbuds from the dollar store for comfort reasons (I have a lot more hair than I did when I got the headset), but I still recommend it, especially for watching TV and not making noise. This headset was especially revolutionary for flights (especially flights with noisy kids, many of which I have had to go on since buying the headset and moving to another country).
