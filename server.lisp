(ql:quickload :wuwei)

(net.aserve:publish-directory :prefix "/election" :destination "/misc/working/election/")

(net.aserve:publish-directory :prefix "/d3" :destination "/misc/reposed/d3/")

(net.aserve:start :port 8888)
