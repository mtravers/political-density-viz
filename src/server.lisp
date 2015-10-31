(ql:quickload :aserve)

;;; Start up a server

(net.aserve:publish-directory :prefix "/election" :destination "~/Dropbox/repos/election/public")
(net.aserve:start :port 8888)

;;; Go to http://localhost:8888/election/scatterstates.html




