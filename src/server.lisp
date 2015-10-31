(ql:quickload :aserve)

(net.aserve:publish-directory :prefix "/election" :destination "~/Dropbox/repos/election/public")
(net.aserve:start :port 8888)
;;; Go to http://localhost:8888/election/scatterstates.html

; (net.aserve:publish-directory :prefix "/d3" :destination "/misc/reposed/d3/")


