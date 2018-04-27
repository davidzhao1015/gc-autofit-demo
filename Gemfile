source 'https://rubygems.org'
ruby '2.2.2'
gem 'rails', '4.2.3'
gem 'sass-rails', '~> 5.0'
gem 'uglifier', '>= 1.3.0'
gem 'coffee-rails', '~> 4.1.0'
gem 'jquery-rails'
gem 'jbuilder', '~> 2.0'
gem 'bootstrap-sass'
gem 'mysql2', '~> 0.3.18'
gem 'puma'
gem 'slim-rails'
gem 'sidekiq'
gem 'sidekiq-status'

gem 'sinatra', :require => false # for sinatra interface
gem 'paperclip'
gem 'rubyzip'
gem 'whenever', :require => false
gem 'google-analytics-rails'
gem 'systemu'
gem 'jquery-datatables-rails', '~> 1.12.2'
# Wishart gems
gem 'wishart', git: 'git@bitbucket.org:wishartlab/wishart'

group :development, :test do
  gem 'byebug'
  gem 'web-console', '~> 2.0'
  gem 'spring'
  gem 'factory_girl_rails'
  gem 'pry-rails'
  gem 'pry-rescue'
  gem 'rspec-rails', '~> 3.0.0'
end

group :development do
  gem 'better_errors'
  # gem 'capistrano', '~> 3.0.1'
  gem 'capistrano'
  gem 'capistrano-bundler'
  gem 'capistrano-sidekiq'
  # gem 'capistrano-rails', '~> 1.1.3'
  gem 'capistrano-rails'
  gem 'capistrano-rails-console'
  gem 'quiet_assets'
  gem 'rails_layout'
  gem 'guard-bundler'
  gem 'guard-rails'
  gem 'guard-sidekiq'
  gem 'awesome_print'
end

group :production do
  #gem 'newrelic_rpm'
  gem 'execjs'
  gem 'therubyracer', require: 'v8'
end
