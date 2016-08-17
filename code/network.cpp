// Stolen from BSD Sockets: A Quick and Dirty Tutorial

#include <errno.h> /* obligatory includes */  
#include <signal.h>  
#include <stdio.h>  
#include <unistd.h>  
#include <sys/types.h>  
#include <sys/socket.h>  
#include <sys/wait.h>  
#include <netinet/in.h> 
#include <netinet/tcp.h> 
#include <netdb.h>
#include <string.h>

int establish(unsigned short portnum) 
{ 
  char myname[256]; 
  int s; 
  struct sockaddr_in sa; 
  struct hostent *hp; 
  memset(&sa, 0, sizeof(struct sockaddr_in)); /* clear our address */ 
  gethostname(myname, 255); /* who are we? */ 
  hp= gethostbyname(myname); /* get our address info */ 
  if (hp == NULL) /* we don't exist !? */ 
    return(-1); 
  sa.sin_family= hp->h_addrtype; /* this is our host address */ 
  sa.sin_port= htons(portnum); /* this is our port number */ 
  if ((s= socket(AF_INET, SOCK_STREAM, 0)) < 0) /* create socket */ 
    return(-1); 
  if (bind(s,(struct sockaddr*)&sa,sizeof(struct sockaddr_in)) < 0) { 
    close(s); 
    return(-1); /* bind address to socket */ 
  }
  int flag=1;
  setsockopt(s, IPPROTO_TCP, TCP_NODELAY, (char*)&flag, sizeof(int));
  setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char*)&flag, sizeof(int));
  listen(s, 3); /* max # of queued connects */ 
  return(s); 
}

int get_connection(int s)
{ 
  int t; /* socket of connection */ 
  if ((t = accept(s,NULL,NULL)) < 0) /* accept connection if there is one */ 
    return(-1); 
  return(t); 
}
