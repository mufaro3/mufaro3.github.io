module FlipAnimationFilter 
  def flip_animate(input)
    array = input.split('')
    array = array.each_with_index.map { | letter, index | "<span style='--i:#{index}'>#{letter}</span>" }
    letter_spans = array.join(' ')
    return "<div class='letter-flip-animation'>#{letter_spans}</div>" 
  end
end

Liquid::Template.register_filter(FlipAnimationFilter)
