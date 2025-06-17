remotes::install_github('hrbrmstr/iptools')
remotes::install_github('rundel/livecode')
# ssh -R 7777:localhost:7777 user@server -p 22
# In /etc/ssh/sshd_config
# GatewayPorts yes
# AllowTcpForwarding yes

remotes::install_github('hrbrmstr/iptools')
remotes::install_github('rundel/livecode')
server <- livecode::serve_file(ip = '127.0.0.1', port = '7777')

livecode::stop_all()

server$send_msg("Oh, you seem to have succeeded", type="Success")
hello
x <- c(1:10)
paste <- function(...) { paste0(...) }
paste('I','hate','spaces!')

viewer <- getOption("viewer", default = utils::browseURL)
viewer('http://livecode.kierczak.net:7777', height = NULL)
