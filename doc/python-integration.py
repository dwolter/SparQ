import socket,sys,time
CRLF = "\r\n"    # Define line endings

def readline():
  "Read a line from the server.  Strip trailing CR and/or LF."
  input = sockfile.readline()
  if not input:
    raise EOFError
  if input[-2:] == CRLF:  # strip line endings
    input = input[:-2]
  elif input[-1:] in CRLF:
    input = input[:-1]
  if len(input) == 0:
    return readline()
  if input[0] == ";":     # ignore comments
    return readline()
  else:            
    return input

def sendline(line):  # send a line to SparQ
  sock.send(line + CRLF) # unbuffered write                                                                                    

def removePrompt( line ):  # remove "sparq>" prompt
    return line[line.find('>')+7:]

# create a socket and connect
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.connect(('localhost', 4443))
sockfile = sock.makefile('rw')

# qualify a geometrical scenario with DRA-24
sendline('qualify dra-24 first2all ((A 4 6 9 0.5) (B -5 5 0 2) (C -4 5 6 0))')
scene = removePrompt( readline() )
print scene

# add an additional network ((B 4_1 C))
sendline("constraint-reasoning dra-24 refine " + scene + ' ((B eses C))')
scene2 = removePrompt( readline(  ) )
print scene2

# check the new scenario for consistency
sendline('constraint-reasoning dra-24 algebraic-closure ' + scene2)
consistent = removePrompt( readline() )
print consistent
if consistent != "Not consistent.": # ...read resulting CSP
    net = readline()
    print net

sendline("quit")
sock.close()
