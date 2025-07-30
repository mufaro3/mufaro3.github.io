---
layout: page
permalink: /all
title: All Posts
---

<div style="width:100%;text-align:right;">
    <a href="{{ site.url }}/category" title="Category List">Category List</a> -
    <a href="{{ site.url }}/rss" title="RSS Feed">RSS Feed</a>
</div>

All of the posts produced on this website, in order by post date!

{% assign last_date = null %}
<div id="post-list">
{% for post in site.posts %}
   {% capture current_date  %}{{ post.date | date: "%B %-d, %Y" }}{% endcapture %}
   {% if current_date != last_date %}
      <br>
      <span class="mono">{ <span class="emph">{{ current_date }}</span> }</span>
      <br/>
   {% endif %}
   <a href="{{ post.url }}">{{ post.title | xml_escape }}</a>
   <br/>
   {% assign last_date = current_date %}
{% endfor %}
</div>