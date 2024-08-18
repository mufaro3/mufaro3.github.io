---
layout: post
title:	"Example Programming Post"
date:	2024-08-12 11:10:00
categories:
    - programming 
tags:
    - examples 
---

$$2+2$$

\begin{equation}
\int_0^\pi \sin (x) dx = 2
\end{equation}

{% highlight common_lisp %}
(defun filter(list predicate)
    (if (null list) '()
        (let ((the-rest (filter (cdr list) predicate)))
            (if (funcall predicate (car list))
                (cons (car list) the-rest)
                the-rest))))
{% endhighlight %}

{% highlight c %}
static __GLOBAL struct config_vars *config;

static __EXAMPLE_LIB_SHARED uint8_t
get_user_token_from_config(struct ini_parser *parser,
                           const FILE *config_file)
{
    char *buffer = parse_ini_field(
        "USERDATA","USERNAME", parser, config_file);
    if (!buffer) {
        return LIB_FUNC_ERR;
    }
    strncpy(config->user_token, strlen(buffer), buffer);
    return LIB_FUNC_OK;
}
{% endhighlight %}
