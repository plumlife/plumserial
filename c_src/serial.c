/*
 Copyright (c) 2014 Plum Inc.
 
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
 *    File:	 serial.c
 *    Author:	 Matt Brandt
 *    Created:	 Thu Nov 6, 2014
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
    ttymode.c_cflag = CS8 | CREAD | CLOCAL;
    
    if (tcsetattr(fd, TCSANOW, &ttymode) < 0)
    {
        perror("tcsetattr");
        exit(1);
    }
}


/**********************************************************************
 * Name: read_exactly(fd, buf, nr)
 * Desc: Read exactly nr bytes and put them into buf. Return the number
 *       of bytes read, i.e. nr.
 * Returns: The number of bytes read, or 0 if stream closed.
 */

int read_exactly(int fd, char buf[], int nr)
{
    int remaining = nr;
    int nr_read = 0;
    
    while(remaining > 0)
    {
        ssize_t n = read(fd, &buf[nr_read], remaining);
        
        if( n == 0 )            /* EOF? */
            return 0;
        remaining -= n;
    }
    
    return nr;
}

int debug = 0;
char *tty_name = "/dev/ttyAMA0";
int speed = 115200;
int ttyfd = -1;

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
    for(;;)
    {
        char buf[4] = { 0, 1, 0, 0 };

        if( read(ttyfd, buf + 2, 1) == 0 )
            exit(0);

        write(1, buf, 3);
    }
}

int main(int argc, char *argv[])
{
    process_args(argc, argv);
    ttyfd = open(tty_name, O_RDWR | O_NOCTTY | O_NONBLOCK);
    if( ttyfd < 0 )
    {
        fprintf(stderr, "Can't open tty %s\n", tty_name);
        exit(1);
    }

    set_tty_mode(ttyfd, speed);

    fcntl(ttyfd, F_SETFL, 0);

    pthread_t reader_id;

    pthread_create(&reader_id, NULL, reader_thread, NULL);

    for(;;)
    {
        char buf[1024];

        if( read_exactly(0, buf, 2) != 2 )
            exit(0);

        int len = (buf[0] << 8) + (buf[1] & 0x0ff);

        if( debug )
            fprintf(stderr, "got length: %d\n", len);

        if( read_exactly(0, buf, len) != len )
            exit(0);

        buf[len] = 0;
        if( debug )
            fprintf(stderr, "got buffer: \"%s\"\n", buf);

        write(ttyfd, buf, len);
    }
    exit(0);
}
