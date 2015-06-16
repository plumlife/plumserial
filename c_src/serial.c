/*
 Copyright (c) 2015 Plum Inc.

 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in
 all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 THE SOFTWARE.
 */

/*
 *    -*- C -*-
 *    File:  serial.c
 *    Author:    Matt Brandt
 *    Created:   Thu Nov 6, 2014
 *    Purpose:   Provide a actual working serial port.
 */

/*
    This module is called as follows:

    Port = serial:start([{tty, "/dev/tty.usbserial"}, {speed, 115200}]).

    The options list to start can contain the following patterns:

    {tty, Name}         set the name of the tty to open (default /dev/ttyAMA0)
    {speed, Speed}      integer baud rate, can be any normal baud rate
 */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <termios.h>
#include <fcntl.h>
#include <errno.h>
#include <ctype.h>
#include <signal.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pthread.h>
#include <unistd.h>

/**********************************************************************
 * Name: get_speed
 *
 * Desc: Returns the speed_t value associated with a given bit_rate
 *       according to the bitrate_table. B0 is returned if no matching entry
 *       is found.
 */

speed_t get_speed(int speed)
{
    switch(speed)
    {
        case 50: return B50;
        case 75: return B75;
        case 110: return B110;
        case 134: return B134;
        case 150: return B150;
        case 200: return B200;
        case 300: return B300;
        case 600: return B600;
        case 1200: return B1200;
        case 1800: return B1800;
        case 2400: return B2400;
        case 4800: return B4800;
        case 9600: return B9600;
        case 19200: return B19200;
        case 38400: return B38400;
        case 57600: return B57600;
        case 115200: return B115200;
        case 230400: return B230400;
    }
    return B0;
}

/**********************************************************************
 * Name: set_raw_tty_mode
 *
 * Desc: Configures the given tty for raw-mode.
 */

void set_tty_mode(int fd, int speed)
{
    struct termios ttymode;
    speed_t spd = get_speed(speed);

    /* Get ttymode */

    if (tcgetattr(fd, &ttymode) < 0)
    {
        perror("tcgetattr");
        exit(1);
    }

    cfmakeraw(&ttymode);
     /* Apply changes */

    cfsetspeed(&ttymode, spd);
    ttymode.c_cflag |= CLOCAL;

    ttymode.c_cc[VMIN] = 1;
    ttymode.c_cc[VTIME] = 0;

    if (tcsetattr(fd, TCSANOW, &ttymode) < 0)
    {
        perror("tcsetattr");
        exit(1);
    }
}

int debug = 0;
char *tty_name = "/dev/ttyAPP0";
int speed = 115200;
int ttyfd = -1;

/*named pipes stuff */
char *bleTX_name = "/etc/init.d/bleTX.fifo";
char *bleRX_name = "/etc/init.d/bleRX.fifo";
int bleTX = -1;
int bleRX = -1;

void process_args(int argc, char **argv)
{
    while( --argc > 0 )
    {
        char *arg = *++argv;

        if( strcmp(arg, "-s") == 0 && argc > 0 )
        {
            speed = atoi(*++argv);
            --argc;
        }
        else if( strcmp(arg, "-d") == 0 )
        {
            debug = 1;
        }
        else
        {
            tty_name = arg;
        }
    }
}

void *reader_thread(void *arg)
{

    int fd = *((int*) arg);

    for(;;)
    {
        char buf[64];

        int len = read(fd, buf, sizeof(buf));
        if( len == 0 )
            exit(0);

        write(1, buf, len);
    }
}


static int named_pipes_exist(){
  if( access( bleTX_name, F_OK ) == -1 ) {
    return 0;
  }
  if( access( bleRX_name, F_OK ) == -1 ) {
    return 0;
  }



  return 1;
}
static int open_named_pipes(){

  /* both exist ? then open them */
  bleTX = open(bleTX_name,O_WRONLY);
  bleRX = open(bleRX_name,O_RDONLY);

  if (bleTX < 0){
    return -1;
  }
  if (bleRX < 0){
    return -1;
  }
  return 0;
}

/*
 * for now make this backwards compatible with the initial dimmer, where we used
 * a serial line to communicate with the TI BLE chip. On the new dimmer, we actually
 * communicate with a daemon (plumGATT) which is attached to the serial port. We use
 * named pipes to communicate with the daemon. TODO: a better way to communicate?
 */
int main(int argc, char *argv[])
{
    process_args(argc, argv);

    /* workaround until we break BLE communication into a seperate module */
    if (named_pipes_exist() && (speed != 115200)){
      fprintf(stderr, "named pipes exist, so assuming neu dimmer + BLE\n");
      if (open_named_pipes() !=0){
        fprintf(stderr, "failed opening named pipes!\n");
        exit(1);
      }
      pthread_t reader_id;
      pthread_create(&reader_id, NULL, reader_thread,&bleRX);
      for(;;)
      {
          char buf[64];

          int len = read(0, buf, sizeof(buf));
          if( len <= 0 )
              exit(0);

              write(bleTX, buf, len);
      }

    }  else {
        ttyfd = open(tty_name, O_RDWR | O_NOCTTY | O_NONBLOCK);
        if( ttyfd < 0 ){
          fprintf(stderr, "old dimmer: Can't open tty %s  \n", tty_name);
          exit(1);
      }

      set_tty_mode(ttyfd, speed);

      fcntl(ttyfd, F_SETFL, 0);

      pthread_t reader_id;

      pthread_create(&reader_id, NULL, reader_thread, &ttyfd);

      for(;;)
      {
          char buf[64];

          int len = read(0, buf, sizeof(buf));
          if( len <= 0 )
              exit(0);

              write(ttyfd, buf, len);
      }
    }
  exit(0);
}
