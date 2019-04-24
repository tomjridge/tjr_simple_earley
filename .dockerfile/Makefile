SHELL:=bash
NAME:=tjr_simple_earley

all: 
	$(MAKE) from_github
	$(MAKE) local
#	$(MAKE) using_opam

local:
	rm -rf $(NAME)
	mkdir $(NAME)
	rsync -vaz ../{Makefile,tjr_simple_earley.opam,bash_env.sh,src,bin} ./$(NAME)
	docker build -t $(NAME)_local -f Dockerfile.local .

from_github:
	docker build -t $(NAME)_from_github -f Dockerfile.from_github .

using_opam:
	docker build -t $(NAME)_using_opam -f Dockerfile.using_opam .

#	docker run -ti $NAME bash
clean:
	-docker image rm $(NAME)_local $(NAME)_from_github $(NAME)_using_opam
	rm -rf $(NAME)
