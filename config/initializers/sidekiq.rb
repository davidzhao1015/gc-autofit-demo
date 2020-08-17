require 'sidekiq'
require 'sidekiq/web'

if Rails.env.development?
	Sidekiq.configure_server do |config|
	  config.redis = { url: 'redis://127.0.0.1:6379/2', network_timeout: 5 }
	end

	Sidekiq.configure_client do |config|
	  config.redis = { :url => 'redis://127.0.0.1:6379/2', network_timeout: 5 }
	end

elsif Rails.env.production?
	Sidekiq.configure_server do |config|
	  config.redis = { url: 'redis://127.0.0.1:6379/2', network_timeout: 5 }
	end

	Sidekiq.configure_client do |config|
	  config.redis = { :url => 'redis://127.0.0.1:6379/2', network_timeout: 5 }
	end

	Sidekiq::Web.use(Rack::Auth::Basic) do |user, password|
	  Rack::Utils.secure_compare(::Digest::SHA256.hexdigest(user), ::Digest::SHA256.hexdigest("wishartlab")) &
	  Rack::Utils.secure_compare(::Digest::SHA256.hexdigest(password), ::Digest::SHA256.hexdigest("</B/3fbn.zxJGXRxUBA5g2ByR9Xe"))
	end
end