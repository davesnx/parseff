project_name = parseff

# SWITCH names an opam switch to use instead of the current one, for example
# SWITCH=oxcaml.5.4.0+ox9 make install test. Each switch builds in its own
# BUILD_DIR.
SWITCH ?=
OPAM_EXEC = opam exec $(if $(SWITCH),--switch=$(SWITCH)) --
BUILD_DIR ?= $(if $(SWITCH),_build-$(SWITCH),_build)
DUNE ?= DUNE_BUILD_DIR=$(BUILD_DIR) $(OPAM_EXEC) dune

# A SWITCH whose name contains "ox" gets OxCaml.
comma := ,
OXCAML_COMPILER = ocaml-variants.5.4.0+ox$(comma)oxcaml-compiler.5.4.0-ox9
COMPILER ?= $(if $(findstring ox,$(SWITCH)),$(OXCAML_COMPILER),5.5.2)
OXCAML_REPOS = ox=git+https://github.com/oxcaml/opam-repository.git,default

# Non-empty when the switch runs OxCaml. Expanded only by the recipes that use it.
IS_OXCAML = $(shell $(OPAM_EXEC) ocamlc -version 2>/dev/null | grep '+ox')

opam_file = $(project_name).opam
PARSEFF_GENERATE_MARKDOWN ?= true

.PHONY: help
help: ## Print this help message
	@echo "";
	@echo "List of available make commands";
	@echo "";
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}';
	@for f in $(BENCH_FILES); do \
		name=$$(basename $$f .ml | sed 's/bench_//'); \
		printf "  \033[36m%-15s\033[0m Run bench-$$name benchmark\n" "bench-$$name"; \
	done;
	@echo "";

.PHONY: build
build: ## Build the project, including non installable libraries and executables
	$(DUNE) build --root . --profile=dev

.PHONY: build-prod
build-prod: ## Build for production (--profile=prod)
	$(DUNE) build --root . --profile=prod

.PHONY: dev
dev: ## Build in watch mode
	$(DUNE) build --root . -w --profile=dev

.PHONY: clean
clean: ## Clean artifacts
	$(DUNE) clean --root .

.PHONY: test
test: ## Run the unit tests; on OxCaml, those in test/ only, and fail on a safety alert in lib/
	@if [ -z "$(IS_OXCAML)" ]; then \
		echo '$(DUNE) build --root . @runtest'; $(DUNE) build --root . @runtest; \
	else \
		out=$$(DUNE_CACHE=disabled $(DUNE) build --root . ./lib/parseff.cmxa 2>&1); status=$$?; \
		if [ -n "$$out" ]; then printf '%s\n' "$$out"; fi; \
		if [ $$status -ne 0 ]; then exit $$status; fi; \
		if printf '%s\n' "$$out" | grep -q '^Alert'; then echo "lib/ has OxCaml alerts"; exit 1; fi; \
		echo '$(DUNE) build --root . @test/runtest'; $(DUNE) build --root . @test/runtest; \
	fi

.PHONY: test-watch
test-watch: ## Run the unit tests in watch mode
	$(DUNE) build --root . @runtest -w

.PHONY: test-promote
test-promote: ## Updates snapshots and promotes it to correct
	$(DUNE) build --root . @runtest --auto-promote

.PHONY: format
format: ## Format the codebase with ocamlformat
	@DUNE_CONFIG__GLOBAL_LOCK=disabled $(DUNE) build --root . @fmt --auto-promote

.PHONY: format-check
format-check: ## Checks if format is correct
	@DUNE_CONFIG__GLOBAL_LOCK=disabled $(DUNE) build --root . @fmt

.PHONY: create-switch
create-switch: ## Create the opam switch (SWITCH, or a local one)
ifeq ($(SWITCH),)
	opam switch create . $(COMPILER) --deps-only --with-test -y
else
	opam switch create $(SWITCH) --packages=$(COMPILER) $(if $(findstring +ox,$(COMPILER)),--repos $(OXCAML_REPOS)) -y
endif

.PHONY: switch
switch:
	@if [ -n "$(SWITCH)" ] && ! opam switch list --short | grep -Fqx '$(SWITCH)'; then \
		$(MAKE) --no-print-directory create-switch; \
	fi

.PHONY: install
install: switch ## Install dependencies; on OxCaml, only those of the unit tests
	$(if $(IS_OXCAML),opam install $(if $(SWITCH),--switch=$(SWITCH)) dune uucp alcotest -y,opam install $(if $(SWITCH),--switch=$(SWITCH)) . --deps-only --with-test --with-doc --with-dev-setup -y)

.PHONY: init
init: create-switch install ## Create a local dev enviroment

BENCH_FILES := $(wildcard bench/bench_*.ml)
BENCH_NAMES := $(patsubst bench/bench_%.ml,bench-%,$(BENCH_FILES))

.PHONY: bench
bench: $(BENCH_NAMES) ## Run all benchmarks

.PHONY: $(BENCH_NAMES)
$(BENCH_NAMES): bench-%: ## Run bench-% benchmark
	@$(DUNE) exec --root . bench/bench_$*.exe --profile=release --display-separate-messages --no-print-directory

.PHONY: subst
subst: ## Run dune substitute
	$(DUNE) subst

.PHONY: docs
docs: ## Generate odoc documentation and markdown
	PARSEFF_GENERATE_MARKDOWN=$(PARSEFF_GENERATE_MARKDOWN) $(DUNE) build --root . @doc @doc-markdown

.PHONY: website-dev
website-dev: docs ## Run website dev server (generated from .mld)
	./website/generate-changelog.sh
	cd website && npm run dev

.PHONY: website-build
website-build: docs ## Build website (generates markdown from .mld then builds Astro)
	./website/generate-changelog.sh
	@if [ ! -d website/node_modules ]; then \
		echo "Installing website dependencies..."; \
		cd website && npm ci; \
	fi
	cd website && npm run build

.PHONY: website-build-versions
website-build-versions: docs ## Build the website for main and every published tag into website/dist
	./website/generate-changelog.sh
	@if [ ! -d website/node_modules ]; then \
		cd website && npm ci; \
	fi
	./website/build-versions.sh

.PHONY: website-preview
website-preview: ## Preview built website
	cd website && npm run preview
