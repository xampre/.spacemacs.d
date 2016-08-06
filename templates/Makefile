CC = gcc
CFLAGS = -Wall

OBJS = `(let ( (files
                (directory-files (file-name-directory (buffer-file-name)) t "\.cc?$")) )
          (dolist (f files)
            (insert (concat (file-name-base f) ".o ") ))
          (backward-delete-char 1) )`
PROGRAM = $1

.PHONY: all clean
$0
all: \$(PROGRAM)

\$(PROGRAM): \$(OBJS)
	\$(CC) \$(CFLAGS) -o \$(PROGRAM) \$(OBJS)

clean:
	rm -rf *.o \$(PROGRAM)
