(ql:quickload :wuwei)

(net.aserve:publish-directory :prefix "/election" :destination "/misc/working/election/")

(net.aserve:start :port 8888)
