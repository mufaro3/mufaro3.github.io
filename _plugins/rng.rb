module Jekyll
  module RandomFilter
    # Use sample to get a random value from an array
    #
    # input - The Array to sample.
    #
    # Examples
    #
    #   random([1, 2, 3, 4, 5])
    #   # => ([2])
    #
    # Returns a randomly-selected item out of an array.
    def random(input)
      input.sample(1)
    end
  end
end

Liquid::Template.register_filter(Jekyll::RandomFilter)
