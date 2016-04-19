require 'sidekiq/web'

Rails.application.routes.draw do

  resources :submissions do
    get 'example', on: :collection
    get 'profile', on: :member
    post 'save_alkane_standards', on: :member
    resources :spectra
  end

  get 'instructions' => 'home#instructions', :as => :instructions

  get "home/download_pdf_sopOrgAcids"
  get "home/download_pdf_sopSerum"

  root :to => "submissions#new"


  mount Wishart::Engine => "/w" , as: 'wishart'

  if Rails.env.production?
    Sidekiq::Web.use Rack::Auth::Basic do |username, password|
      username == 'admin' && password == 'c4odt8gqLWcfwMCH82'
    end
  end
  mount Sidekiq::Web, at: '/sidekiq'
end
