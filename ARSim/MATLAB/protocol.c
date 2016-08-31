#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "protocol.h" // what
#include "debugm.h"

char PROTOCOL_MAGIC_NUMBERS[2] = {0, 1};

/* == FUNCTION Protocol_send_byte =============================================
 * Transfer one byte to the simulator.
 *
 * If the write is successful, the number of bytes transferred is returned.
 * Otherwise -1 is returned. 
 */
int Protocol_send_byte(Protocol *p, void *buf) 
{
  if (p == NULL)   error(PROTOCOL_MEM_ERROR, "Protocol struct was NULL.");
  if (buf == NULL) error(MEM_ERROR, "Buffer pointer was NULL.");
  
  return write(p->p_input_fildes, (char *) buf, 1);
}

/* == FUNCTION Protocol_send_bytes ============================================
 * Transfer some bytes to the simulator.
 *
 * If the write is successful, the number of bytes transferred is returned.
 * Otherwise -1 is returned. 
 */
int Protocol_send_bytes(Protocol *p, void *buf, int bytes) 
{
  if (p == NULL)   error(PROTOCOL_MEM_ERROR, "Protocol struct was NULL.");
  if (buf == NULL) error(MEM_ERROR, "Buffer pointer was NULL.");
  
  return write(p->p_input_fildes, (char *) buf, bytes);
}

/* == FUNCTION Protocol_get_byte ==============================================
 * Receive one byte from the simulator.
 *
 * If the read is successful, the number of bytes transferred is returned.
 * Otherwise -1 is returned. 
 */
int Protocol_get_byte(Protocol *p, void *buf) 
{
  if (p == NULL)   error(PROTOCOL_MEM_ERROR, "Protocol struct was NULL.");
  if (buf == NULL) error(MEM_ERROR, "Buffer pointer was NULL.");

  return read(p->p_output_fildes, (char *) buf, 1);
}

/* == FUNCTION Protocol_get_bytes =============================================
 * Receive some bytes from the simulator.
 *
 * If the read is successful, the number of bytes transferred is returned.
 * Otherwise -1 is returned. 
 */
int Protocol_get_bytes(Protocol *p, void *buf, int bytes) 
{
  if (p == NULL)   error(PROTOCOL_MEM_ERROR, "Protocol struct was NULL.");
  if (buf == NULL) error(MEM_ERROR, "Buffer pointer was NULL.");

  return read(p->p_output_fildes, (char *) buf, bytes);
}

/* == FUNCTION Protocol_handshake =============================================
 * Performs the handshake procedure:
 * 
 * 1. Send one byte (P_OK) to Haskell.
 * 2. Receive one byte for input port width.
 * 3. Receive one byte for output port width.
 * 4. Receive one byte for the number of input labels (this should
 *    correspond to the number of input ports or something has gone
 *    wrong).
 * 5. Receive input labels:
 *    + Receive label length (NOTE: NOT null terminated).
 *    + Receive label.
 * 6. Repeat steps 4 to 5 for output labels.
 *
 * Since labels are not null terminated we allocate one extra byte using calloc
 * for labels.
 */
int Protocol_handshake(Protocol *p)
{
  if (p == NULL) error(PROTOCOL_MEM_ERROR, "Protocol struct was NULL."); 
 
  if (Protocol_send_byte(p, P_OK) < 0) 
    error(PROTOCOL_ERROR, "Failed to send OK.");

  if (Protocol_get_byte(p, &p->p_input_width) < 1)   
    error(PROTOCOL_ERROR, "Failed to get input width.");

  if (Protocol_get_byte(p, &p->p_output_width) < 1)
    error(PROTOCOL_ERROR, "Failed to get output width.");

  debug("Received sizes: OK.");

  //-- Input labels ---------------------------------------------------------//
  if (Protocol_get_byte(p, &p->p_input_labels) < 1)  
    error(PROTOCOL_ERROR, "Failed to get input label count.");
  
  debug("Received input label count %u: OK", p->p_input_labels);

  p->p_input_labels_str = malloc(p->p_input_labels * sizeof(char *));
  if (p->p_input_labels_str == NULL) 
    error(PROTOCOL_MEM_ERROR, "Failed to allocate space for input labels.");

  for (uint8_t i = 0; i < p->p_input_labels; i++) {
    uint8_t str_len;
    Protocol_get_byte(p, &str_len);
    
    p->p_input_labels_str[i] = calloc(1, (str_len + 1) * sizeof(char));
    if (p->p_input_labels_str[i] == NULL) {
      cleanup(p->p_input_labels_str, i);
      error(PROTOCOL_MEM_ERROR, "Failed to allocate space for input label %u."
           , i);
    }

    if (Protocol_get_bytes(p, p->p_input_labels_str[i], str_len) < 0) {
      cleanup(p->p_input_labels_str, i);
      error(PROTOCOL_ERROR, "Failed to get input label %u.", i);
    }
    debug("LABEL %u: %s.", i, p->p_input_labels_str[i]);

  }

  //-- Output labels --------------------------------------------------------//
  if (Protocol_get_byte(p, &p->p_output_labels) < 1)
    error(PROTOCOL_ERROR, "Failed to get output label count.");

  debug("Received output label count %u: OK", p->p_output_labels);

  p->p_output_labels_str = malloc(p->p_output_labels * sizeof(char *));
  if (p->p_output_labels_str == NULL) {
    cleanup(p->p_input_labels_str, p->p_input_labels);
    error(PROTOCOL_MEM_ERROR, "Failed to allocate space for output labels.");
  }

  for (uint8_t i = 0; i < p->p_output_labels; i++) {
    uint8_t str_len;
    Protocol_get_byte(p, &str_len);
    
    p->p_output_labels_str[i] = calloc(1, (str_len + 1) * sizeof(char));
    if (p->p_output_labels_str[i] == NULL) {
      cleanup(p->p_input_labels_str, p->p_input_labels);
      cleanup(p->p_output_labels_str, i);
      error(PROTOCOL_MEM_ERROR, "Failed to allocate space for output label %u."
           , i);
    }

    if (Protocol_get_bytes(p, p->p_output_labels_str[i], str_len) < 0) {
      cleanup(p->p_input_labels_str, p->p_input_labels);
      cleanup(p->p_output_labels_str, i);
      error(PROTOCOL_ERROR, "Failed to get output label %u.", i);
    }
    debug("LABEL %u: %s.", i, p->p_output_labels_str[i]);
  }
  debug("Received output labels: OK.");

  return PROTOCOL_SUCCESS;
}

/* == FUNCTION Protocol_init ==================================================
 * Creates a new protocol state. Provide with file descriptors for input and
 * output. NOTE: These are confusingly named to correspond with the Haskell side
 * point of view, so that fildes_in is an outgoing O_WRONLY file descriptor, and 
 * vice versa.
 */
Protocol *Protocol_init(int fildes_in, int fildes_out)
{
  Protocol *p = calloc(1, sizeof(Protocol));
  if (p == NULL) error(NULL, "call to calloc failed.");

  p->p_input_fildes  = fildes_in;
  p->p_output_fildes = fildes_out;

  return p;
}

/* == FUNCTION Protocol_destroy ===============================================
 * Cleans up the protocol state.
 */
void Protocol_destroy(Protocol *p)
{
  for (uint8_t i = 0; i < p->p_input_labels; i++)
    free(p->p_input_labels_str[i]);

  for (uint8_t i = 0; i < p->p_output_labels; i++)
    free(p->p_output_labels_str[i]);

  free(p->p_input_labels_str);
  free(p->p_output_labels_str);
  debug("Cleaned up labels.");

  free(p);
}

/* == FUNCTION Protocol_send_data =============================================
 * Transfers data to ARSIM.
 *
 * NOTE: Removed time, since ARSim does not need it.
 */
int Protocol_send_data(Protocol *p, const double *data)
{
  if (Protocol_send_byte(p, P_OK) < 1) 
    error(PROTOCOL_ERROR, "Failed when sending OK.");

  debug("Sent status.");

//  if (Protocol_send_bytes(p, &time, sizeof(double)) < sizeof(double))
//    error(PROTOCOL_ERROR, "Failed when sending timestamp.");
//
//  debug("Sent timestamp %e", time);

  if (Protocol_send_bytes(p, (void *) data, p->p_input_width * sizeof(double)) < 
      p->p_input_width * sizeof(double))
    error(PROTOCOL_ERROR, "Failed when sending data vector.");
  
  debug("Sent data.");

  return PROTOCOL_SUCCESS;
}

/* == FUNCTION Protocol_get_data ==============================================
 * Receives data from ARSIM.
 */
int Protocol_get_data(Protocol *p, double *delta, double *data)
{
  char status = 0;
  if (Protocol_get_byte(p, &status) < 1) 
    error(PROTOCOL_ERROR, "Failed receiving status.");

  debug("Received status %d.", status);

  if (status != 0)
    error(PROTOCOL_HALT, "ARSIM requested halt.");

  if (Protocol_get_bytes(p, delta, sizeof(double)) < sizeof(double))
    error(PROTOCOL_ERROR, "Failed receiving next sample time.");

  debug("Received DELTA: %e", *delta);

  if (Protocol_get_bytes(p, data, p->p_output_width * sizeof(double)) < 
      p->p_output_width * sizeof(double))
    error(PROTOCOL_ERROR, "Failed receiving data vector.");

  debug("Received data.");

  return PROTOCOL_SUCCESS;
}
/* == FUNCTION cleanup ========================================================
 * Helper for cleaning up string arrays on failure.
 */
void cleanup(char **ptr, uint8_t successes) 
{
  for (uint8_t i = 0; i < successes; i++) 
    free(ptr[i]);
  free(ptr);
}
