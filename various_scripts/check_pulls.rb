#!/usr/bin/env ruby

require 'rest-client'
require 'json'

repos = %x(find * -maxdepth 0 -type d).split("\n")
repos.delete "foreman_installer"
repos.each do |repo|
  pulls = JSON.parse RestClient.get "https://api.github.com/repos/theforeman/puppet-#{repo}/pulls"
  next if pulls.size == 0
  puts "puppet-#{repo} has #{pulls.size} open requests"
  pulls.each do |pull|
    user = pull["head"]["user"]["login"]
    merge = pull["head"]["repo"]["clone_url"]
    branch = pull["head"]["ref"]

    puts "  #{pull["title"]}"
    puts "    #{pull["html_url"]}"
    puts "    #{user}: git pull #{merge} #{branch}"
  end
end
