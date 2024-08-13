require 'liquid'
require_relative '../_plugins/animator.rb'

puts FlipAnimationFilter.redify('abcd')
puts FlipAnimationFilter.flip_animate('abcd')
