---
layout: default 
title: "Category List"
permalink: /category
---

<header>
  <h2>Category List</h2>
</header>

<ul class="tag-box inline">
{% assign list = site.tags | sort %}
  {% for category in list %} 
    <li>
      <a href="#{{ category[0] }}">
          {{ category[0] }}
      </a>
      <span>({{ category[1].size }})</span>
    </li>
  {% endfor %}
{% assign list = nil %}
</ul>

{% assign taglist = site.tags | sort %}
{% for category in taglist %} 
  <h3 id="{{ category[0] }}">{{ category[0] }}</h3>
  <ul class="post-list">
  {% assign list = category[1] %}  
  {% for post in list %}
    <li>
    <a href="{{ post.url }}">{{ post.title }}</a>
    </li>
  {% endfor %}
  {% assign pages_list = nil %}
  {% assign group = nil %}
  </ul>
{% endfor %}
{% assign taglist = nil %}
