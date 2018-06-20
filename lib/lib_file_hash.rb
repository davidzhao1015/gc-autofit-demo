module LibFileHash
  def self.included(base)
    base.extend(ClassMethods)    
  end
  
  module ClassMethods
     def get_lib_file_dict(dir)
      file_list = Dir.glob("#{dir}/*.csv")
      file_hash = file_list.map do |f|
              f_b = File.basename(f)
              if f_b =~/lib_(\w+?)_CalibrationCurve_?(.*).csv/ # for calibration
                type = "#{$1}#{$2}".downcase
                [type, f]
              elsif f_b =~/lib_(\w+?).csv/ # for mz_intensity
                [$1.downcase, f]
              end
            end.to_h        
    end
  end
end 