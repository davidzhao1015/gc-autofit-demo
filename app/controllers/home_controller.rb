class HomeController < ApplicationController

    def download_pdf_sopSerum
        send_file(
            "#{Rails.root}/public/docs/SOP_Serum_for_GC-MS.pdf",
            filename: "SOP_Serum_GC-MS.pdf",
            type: "application/pdf"
        )
    end

    def download_pdf_sopOrgAcids
        send_file(
            "#{Rails.root}/public/docs/SOP_Urine_Org_Acids_GC-MS.pdf",
            filename: "SOP_Org_Acids_GC-MS.pdf",
            type: "application/pdf"
        )
    end
end
