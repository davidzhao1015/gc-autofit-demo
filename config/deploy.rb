#require 'puma/capistrano'
require 'rails'

set :application, 'gc-autofit'
set :repo_url,  "git@bitbucket.org:wishartlab/gc-autofit.git"
set :scm, :git
set :deploy_to, '/apps/gcms/project'
set :use_sudo, false
set :linked_files, %w{config/database.yml}
set :linked_dirs, %w{public/system log gcmsjobs tmp}
set :keep_releases, 3
set :sidekiq_config, "#{Rails.root}/config/sidekiq.yml"
set :sidekiq_pid,  File.join('/', 'tmp', 'gc-autofit.sidekiq.pid')

set :rbenv_map_bins, %w{rake gem bundle ruby rails sidekiq sidekiqctl}

set :branch, ENV['BRANCH'] if ENV['BRANCH']


namespace :deploy do

  desc 'Start application'
  task :start do
    on roles(:web) do
      within release_path do
        execute "script/puma.sh", "start"
      end
    end
  end

  desc 'Stop application'
  task :stop do
    on roles(:web) do
      within release_path do
        execute "script/puma.sh", "stop"
      end
    end
  end

  desc 'Restart application'
  task :restart do
    on roles(:web) do
      # Your restart mechanism here, for example:
      # execute :touch, release_path.join('tmp/restart.txt')
      within release_path do
        execute "script/puma.sh", "restart"
      end
    end
  end

  after :restart, :clear_cache do
    on roles(:web) do
      # Here we can do anything such as:
      # within release_path do
      #   execute :rake, 'cache:clear'
      # end
    end
  end

  after :finishing, 'deploy:cleanup'
  after :finished, :restart

end
