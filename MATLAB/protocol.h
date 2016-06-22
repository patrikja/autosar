#ifndef _PROTOCOL_H
#define _PROTOCOL_H

#include <stdint.h>

/* PROTOCOL MAGIC NUMBERS */
#define P_DIE &PROTOCOL_MAGIC_NUMBERS[1]
#define P_OK  &PROTOCOL_MAGIC_NUMBERS[0]
extern char PROTOCOL_MAGIC_NUMBERS[];

#define PROTOCOL_SUCCESS    0
#define PROTOCOL_MEM_ERROR -2
#define MEM_ERROR          -3
#define PROTOCOL_ERROR     -4
#define PROTOCOL_HALT      -5

/* Some basic state carried by the data transfer system. */
typedef struct Protocol {
  int p_input_fildes;
  int p_output_fildes;

  uint8_t p_input_width;
  uint8_t p_output_width;

  uint8_t p_input_labels;
  uint8_t p_output_labels;
  char **p_input_labels_str;
  char **p_output_labels_str;
} Protocol;


/* Primitive protocol operations: */
Protocol *Protocol_init(int, int);

void Protocol_destroy(Protocol *);
int  Protocol_send_byte(Protocol *, void *);
int  Protocol_send_bytes(Protocol *, void *, int);
int  Protocol_get_byte(Protocol *, void *);
int  Protocol_get_bytes(Protocol *, void *, int);

/* Derived protocol operations: */
int Protocol_handshake(Protocol *);
int Protocol_send_data(Protocol *, const double *);
int Protocol_get_data(Protocol *, double *, double *);

/* Helpers: */
void cleanup(char **, uint8_t);

#endif
