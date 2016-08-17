#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <unistd.h>

#define WALL 'X'
#define EMPTY ' '
#define DOT '.'

#define DIR_UP 0
#define DIR_RIGHT 1
#define DIR_DOWN 2
#define DIR_LEFT 3

#define CMD_LEFT 'L'
#define CMD_RIGHT 'R'
#define CMD_FORW 'F'
#define CMD_SHOOT 'S'
#define CMD_SKIP 'X'

#define PORTNUM 12345
char *DIRS="^>v<x";
char *DIRS2="urdlx";

int establish(unsigned short portnum);
int get_connection(int s);

//int socket_filedes1, socket_filedes2;
FILE*sfp1, *sfp2, *sfp3;

class game {
public:
  game(FILE* sfp1, FILE* sfp2, FILE* sfp3) 
  {
    socket_fp1=sfp1; socket_fp2=sfp2; socket_fp3=sfp3;
  }

  FILE*socket_fp1, *socket_fp2, *socket_fp3;

  int map_height;
  int map_width;
  char **map;
  int robot_cnt;
  int *robot_x, *robot_y, *robot_nx, *robot_ny, 
      *robot_d, *robot_score, *robot_cmd, *robot_dead;

  int robotAt(int x, int y)
  {
    int i;
    for(i=0; i<robot_cnt; i++)
      if((robot_x[i]==x) &&
	 (robot_y[i]==y) &&
	 !robot_dead[i])
	return i;
    return -1;
  }
  
  void readMap()
  {
    FILE*mapfp=fopen("map.dat", "r");
    fscanf(mapfp, "%i %i\n", &map_width, &map_height);
    map=(char**)malloc(sizeof(char*)*map_height);
    int i; 
    for(i=0; i<map_height; i++) {
      putchar('.');
      map[i]=(char*)malloc(sizeof(char)*(map_width+5));
      fgets(map[i], map_width+2, mapfp);
    }
    fscanf(mapfp, "%i", &robot_cnt);
    robot_x=(int*)malloc(sizeof(int)*robot_cnt);
    robot_y=(int*)malloc(sizeof(int)*robot_cnt);
    robot_nx=(int*)malloc(sizeof(int)*robot_cnt);
    robot_ny=(int*)malloc(sizeof(int)*robot_cnt);
    robot_d=(int*)malloc(sizeof(int)*robot_cnt);
    robot_score=(int*)calloc(sizeof(int)*robot_cnt, 1);
    robot_dead=(int*)calloc(sizeof(int)*robot_cnt, 1);
    robot_cmd=(int*)malloc(sizeof(int)*robot_cnt);
    for(i=0; i<robot_cnt; i++) {
      fscanf(mapfp, "%i %i %i", &robot_x[i], &robot_y[i], &robot_d[i]);
    }
    fclose(mapfp);
  }


  void markDead(int x, int y)
  {
    int i;
    if((i=robotAt(x,y))!=-1)
      robot_dead[i]=-1;
  }
  
  void updateMap() 
  {
    int i,j;
    // Get points
    for(i=0; i<robot_cnt; i++) {
      if(!robot_dead[i] && (map[robot_y[i]][robot_x[i]]=='.')) {
	map[robot_y[i]][robot_x[i]]=' ';
	robot_score[i]++;
      }
    }
    
    // Handle rotates
    for(i=0; i<robot_cnt; i++) {
      if(robot_cmd[i]==CMD_LEFT)
	robot_d[i]+=3;
      if(robot_cmd[i]==CMD_RIGHT)
	robot_d[i]+=1;
      robot_d[i]%=4;
    }
    
    // Handle moves
    // Calculate desired next positions
    for(i=0; i<robot_cnt; i++) {
      if(robot_cmd[i]==CMD_FORW) {
	if(robot_d[i]==DIR_UP) {
	  robot_nx[i]=robot_x[i];
	  robot_ny[i]=robot_y[i]-1;
	} else if(robot_d[i]==DIR_DOWN) {
	  robot_nx[i]=robot_x[i];
	  robot_ny[i]=robot_y[i]+1;
	} else if(robot_d[i]==DIR_LEFT) {
	  robot_nx[i]=robot_x[i]-1;
	  robot_ny[i]=robot_y[i];
	} else { // DIR_RIGHT
	  robot_nx[i]=robot_x[i]+1;
	  robot_ny[i]=robot_y[i];
	}
      }
      else {
	robot_nx[i]=robot_x[i];
	robot_ny[i]=robot_y[i];
      }
    }
    // Update to next positions only if no obstacles
    int *next_x=(int*)malloc(sizeof(int)*robot_cnt);
    int *next_y=(int*)malloc(sizeof(int)*robot_cnt);
    for(i=0; i<robot_cnt; i++) {
      int obstacle=(map[robot_ny[i]][robot_nx[i]]=='X'); 
      for(j=0; j<robot_cnt; j++) {
	if((i!=j)&&!robot_dead[j]) {
	  if((robot_nx[i]==robot_x[j])&&(robot_ny[i]==robot_y[j]))
	    obstacle=1;
	  if((robot_nx[i]==robot_nx[j])&&(robot_ny[i]==robot_ny[j]))
	    obstacle=1;
	}
      }
      if(obstacle) {
	next_x[i]=robot_x[i];
	next_y[i]=robot_y[i];
      } else{
	next_x[i]=robot_nx[i];
	next_y[i]=robot_ny[i];
      }
    }
    memcpy(robot_x, next_x, sizeof(int)*robot_cnt);
    memcpy(robot_y, next_y, sizeof(int)*robot_cnt);
    
    // Handle lasers -- first loop marks robots as dying, second as dead. 
    for(i=0; i<robot_cnt; i++) {
      if((robot_dead[i]!=1)&&(robot_cmd[i]==CMD_SHOOT)) {
	int x=robot_x[i];
	int y=robot_y[i];
	if(robot_d[i]==DIR_UP) {
	  for(y=y-1; map[y][x]!='X'; y--)
	    markDead(x,y);
	} else if(robot_d[i]==DIR_DOWN) {
	  for(y=y+1; map[y][x]!='X'; y++)
	    markDead(x,y);
	} else if(robot_d[i]==DIR_LEFT) {
	  for(x=x-1; map[y][x]!='X'; x--)
	    markDead(x,y);
	} else if(robot_d[i]==DIR_RIGHT) {
	  for(x=x+1; map[y][x]!='X'; x++)
	    markDead(x,y);
	}
      }
    }
    // Mark dying robots as dead
    for(i=0; i<robot_cnt; i++) {
      robot_dead[i]=!!robot_dead[i];
    }
  }
  
  void printMap(FILE*f, int swap, int diff) 
  {
    int i,j;
    fprintf(f,"Score");
    for(i=0; i<robot_cnt; i++)
      fprintf(f," %i", robot_score[i]);
    fputc('\n', f);
    for(i=0; i<map_height; i++) {
      for(j=0; j<map_width; j++) {
	int r;
	if((r=robotAt(j,i))!=-1) {
	  if(!diff || r<(robot_cnt/2))
	    fputc(DIRS[robot_d[r]], f);
	  else
	    fputc(DIRS2[robot_d[r]], f);
	}
	else
	  fputc(map[i][j], f);
      }
      fputc('\n', f);
    }
    for(i=0; i<robot_cnt; i++) {
      int k=(i+swap*robot_cnt/2)%robot_cnt;
      fprintf(f, "%i %i %i %i %i\n", i, robot_x[k], robot_y[k], robot_d[k], 
	      robot_dead[k]);
    }
    fprintf(f,"DONE\n");
    fflush(f);
  }

  void dataWait(FILE*f)
  {
    int fd=fileno(f);
    int r; 
    fd_set rd_set;

    do{
      FD_ZERO(&rd_set);
      FD_SET(fd, &rd_set);
      r=select(fd+1, &rd_set, 0,0,0);
    } while((r==-1) && (errno=EINTR));
  }

  void readSocket(FILE*f, int robot_number)
  {
    int i; 
    int*buf=robot_cmd+robot_number*robot_cnt/2;
    for(i=0; i<robot_cnt/2; i++) {
      if(feof(f)) {
	dataWait(f);
      }

      //while(feof(f))
      //;
      while(!isalpha(buf[i]=fgetc(f))) // skip newlines and crap
	;
      putchar(buf[i]);
    }
    putchar('\n');
    fprintf(f,"Commands: ["); 
    for(i=0; i<robot_cnt/2; i++)
      fputc(buf[i], f);
    fprintf(f,"]\n");
  }

  void iterate() 
  {
    printf("Robot 1 about to run:");
    system("date");
    printMap(socket_fp1, 0,0);
    readSocket(socket_fp1, 0);
    printf("Robot 2 about to run:");
    system("date");
    printMap(socket_fp2, 1,0);
    readSocket(socket_fp2, 1);
    printf("Robot 2 finished:");
    system("date");
    printMap(socket_fp3, 0,1);
    printMap(stdout, 0, 1);
    updateMap();
    system("sleep .25s");
  }

  void run()
  {
    readMap();

    while(1) {
      iterate();
    }
  }
};

void*gameRunner(void*v)
{
  game *g=(game*)v;
  g->run();
}

void initSockets() 
{
  int s, t1, t2, t3; 
  if ((s= establish(PORTNUM)) < 0) {
    perror("establish"); 
    exit(1); 
  }
  
  while(1) {
    if ((t1= get_connection(s)) < 0) { /* get a connection */ 
      if (errno == EINTR) { /* EINTR might happen on accept(), */ 
	perror("Add retry code to connection");
	exit(1); /* We should try again */
      }
      perror("accept"); /* bad */ 
      exit(1); 
    }
    if ((t2= get_connection(s)) < 0) { /* get a connection */ 
      if (errno == EINTR) { /* EINTR might happen on accept(), */ 
	perror("Add retry code to connection");
	exit(1); /* We should try again */
      }
      perror("accept"); /* bad */ 
      exit(1); 
    }
    if ((t3= get_connection(s)) < 0) { /* get a connection */ 
      if (errno == EINTR) { /* EINTR might happen on accept(), */ 
	perror("Add retry code to connection");
	exit(1); /* We should try again */
      }
      perror("accept"); /* bad */ 
      exit(1); 
    }

    sfp1=fdopen(t1, "r+");
    sfp2=fdopen(t2, "r+");
    sfp3=fdopen(t3, "r+");
    setvbuf(sfp1, NULL, _IONBF, 0); // turn off buffering
    setvbuf(sfp2, NULL, _IONBF, 0); // turn off buffering
    game g(sfp1, sfp2, sfp3);
    
    pthread_t pt;

    pthread_create(&pt, NULL, gameRunner, (void*)&g);
  }
}


int main()
{
  initSockets();
  putchar('.');

  return 0;
}
