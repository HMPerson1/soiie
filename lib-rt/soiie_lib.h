#ifndef SOIIE_LIB_H
#define SOIIE_LIB_H

#include <stdlib.h>
#include <limits.h>
#include <stdio.h>
#include <errno.h>

extern const int PARAM_COUNT;

void get_params(int argc, char* argv[], long int params[]);
void print(long int x);

#endif
