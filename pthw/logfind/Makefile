NAME		=	logfind
CC			=	buildapp
MANIFEST 	= 	--manifest-file quicklisp-manifest.txt
LIBS		=	--load-system 	cl-ppcre				\
				--load-system	sb-posix				\
				--load-system	sb-bsd-sockets			
SRC			=	--load			logfind.lisp			
OPTIONS		=	--entry			main					\
				--output		${NAME}

${NAME}:    manifest
			${CC} ${MANIFEST} ${LIBS} ${SRC} ${OPTIONS}

manifest:
			sbcl --no-userinit --no-sysinit --non-interactive 	\
				--load ~/quicklisp/setup.lisp 					\
                --eval '(require :sb-posix)' 					\
                --eval '(ql:quickload "cl-ppcre")' 				\
                --eval '(ql:write-asdf-manifest-file "quicklisp-manifest.txt")'

all:		${NAME}

clean:
			@rm -fv logfind
			@rm -fv quicklisp-manifest.txt

re:			clean all

install:	
			@echo 'cp ${NAME} to /usr/local/bin'
			@cp ${NAME} /usr/local/bin/
			@echo 'Just paste any pcre (mathing files, like .*.lisp for instance but feel free to test the limit)'
			@echo 'in ~/.logfind and start logfinding'

.PHONY:		all
