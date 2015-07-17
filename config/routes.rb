require 'sidekiq/web'

Rails.application.routes.draw do

  resources :spectra
  resources :submissions

  root :to => "submissions#new"


  mount Wishart::Engine => "/w" , as: 'wishart'

  if Rails.env.production?
    Sidekiq::Web.use Rack::Auth::Basic do |username, password|
      username == 'admin' && password == 'c4odt8gqLWcfwMCH82'
    end
  end
  mount Sidekiq::Web, at: '/sidekiq'
end
