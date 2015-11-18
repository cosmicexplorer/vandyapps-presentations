# run with:
# coffee server.coffee

net = require 'net'
dgram = require 'dgram'

msgs = ['Success', 'Failure', 'lmao', 'wtf']
contents = ['cheese', 'buffalo', 'pizza']

getRandomEl = (arr) -> arr[Math.round(Math.random() * (arr.length - 1))]
getRandomMsg = ->
  subj = getRandomEl msgs
  cont = getRandomEl contents
  "#{subj}: #{cont}\n"

tcpServer = net.createServer((sock) ->
  console.log 'tcp client connected'
  sock.write getRandomMsg()
  sock.end()).listen 8080

udpServer = dgram.createSocket('udp4').on('message', (msg, rinfo) ->
  console.log 'udp client connected'
  msg = getRandomMsg()
  udpServer.send msg, 0, msg.length, rinfo.port, rinfo.address)
  .bind 8081
