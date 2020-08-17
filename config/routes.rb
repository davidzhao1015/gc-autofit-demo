require 'sidekiq/web'
Rails.application.routes.draw do

  mount Sidekiq::Web => '/sidekiq'

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
  
  namespace :lib do
    get 'mz_db', to: 'csv#mz_db'
    get 'calibration_db', to: 'csv#calibration_db'
    
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
  
end
