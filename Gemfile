source 'https://rubygems.org'
ruby '2.7.1'

gem 'rails', '5.2.0'
gem 'sass-rails', '~> 5.0'
gem 'uglifier', '>= 1.3.0'
gem 'coffee-rails'
gem 'jquery-rails'
gem 'jbuilder', '~> 2.0'
gem 'bootstrap-sass'
gem 'mysql2', '~> 0.5.3'
gem 'slim-rails'
gem 'sidekiq'

gem 'sinatra', :require => false # for sinatra interface
gem 'paperclip', git: 'https://github.com/sd/paperclip', branch: 'remove-mimemagic'
gem 'rubyzip'
gem 'whenever', :require => false
gem 'google-analytics-rails'
gem 'systemu'
gem 'jquery-turbolinks'
gem 'turbolinks'
gem 'mimemagic', git: 'https://github.com/mimemagicrb/mimemagic.git', ref: '01f92d86d15d85cfd0f20dabd025dcbd36a8a60f'

# Wishart
gem 'wishart', git: 'git@bitbucket.org:wishartlab/wishart', branch: 'rails5.2'


group :development, :test do
  gem 'byebug'
  gem 'spring'
  gem 'pry-rails'
  gem 'pry-rescue'
  gem 'rspec-rails', '~> 3.5'
end

group :development do
  gem 'better_errors'
  gem 'capistrano'
  gem 'capistrano-bundler'
  gem 'capistrano-sidekiq'
  gem 'capistrano-rails'
  gem 'capistrano-rails-console'
  gem 'capistrano-rbenv'
  gem 'rails_layout'
  gem 'guard-bundler'
  gem 'guard-rails'
  gem 'guard-sidekiq'
  gem 'awesome_print'
  gem 'ed25519'
  gem 'bcrypt_pbkdf'
end

group :production do
  gem 'puma', '~> 4.3.5'
  gem 'puma_worker_killer', '~> 0.2.0'
  gem 'execjs'
  # gem 'therubyracer', require: 'v8'
end
