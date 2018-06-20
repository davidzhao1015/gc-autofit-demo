require 'sidekiq/web'

Rails.application.routes.draw do


  resources :submissions do
    get 'example', on: :collection
    get 'profile', on: :member
    post 'save_alkane_standards', on: :member
    resources :spectra
  end

  get 'instructions' => 'home#instructions', :as => :instructions
  get 'inst_userownlib' => 'home#inst_userownlib', :as => :inst_userownlib

  get "home/download_pdf_sopOrgAcids"
  get "home/download_pdf_sopSerum"

  root :to => "submissions#new"
  
  namespace :admin do
    namespace :db do
      resources :csv
    end

    namespace :calibration do
      resources :csv
    end
    
    get '',  to: 'admin#index'
  end

  namespace :lib do
    namespace :db do
      get '', to: 'csv#index'
    end

    namespace :calibration do
      get '', to: 'csv#index'
    end
      
    get '',  to: 'lib#index'
  end

  namespace :makedb do
    namespace :db do
      resources :csv  do
        collection do
          get 'download'
        end
      end
    end

    namespace :calibration do
      resources :csv  do
        collection do
          get 'download'
        end
      end
    end
    
    get '',  to: 'makedb#index'
  end

  mount Wishart::Engine => "/w" , as: 'wishart'

  if Rails.env.production?
    Sidekiq::Web.use Rack::Auth::Basic do |username, password|
      username == 'admin' && password == 'c4odt8gqLWcfwMCH82'
    end
  end
  mount Sidekiq::Web, at: '/sidekiq'
end
