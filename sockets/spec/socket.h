#include <errno.h> /* errno, E constants */
#include <string.h> /* strerror() */
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h> /* close() */

// autowrap:c-aref uses make-instance which doesn't work on types like
// (:struct (ffi:iovec)):
typedef struct iovec iovec;
