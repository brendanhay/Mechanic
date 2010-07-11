#!/usr/bin/ruby
require 'webrick'
include WEBrick

dir = "#{Dir::pwd}/priv/www"
port = 8000

print dir, "\n"

#port = 8000 + (dir.hash % 1000)

puts "URL: http://#{Socket.gethostname}:#{port}"

s = HTTPServer.new(
    :Port            => port,
    :DocumentRoot    => dir
)

trap("INT" ){ s.shutdown }

s.start
