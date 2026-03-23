project_name = parseff

DUNE = opam exec -- dune
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
	$(DUNE) build --profile=dev

.PHONY: build-prod
build-prod: ## Build for production (--profile=prod)
	$(DUNE) build --profile=prod

.PHONY: dev
dev: ## Build in watch mode
	$(DUNE) build -w --profile=dev

.PHONY: clean
clean: ## Clean artifacts
	$(DUNE) clean

.PHONY: test
test: ## Run the unit tests
	$(DUNE) build @runtest

.PHONY: test-watch
test-watch: ## Run the unit tests in watch mode
	$(DUNE) build @runtest -w

.PHONY: test-promote
test-promote: ## Updates snapshots and promotes it to correct
	$(DUNE) build @runtest --auto-promote

.PHONY: format
format: ## Format the codebase with ocamlformat
	@DUNE_CONFIG__GLOBAL_LOCK=disabled $(DUNE) build @fmt --auto-promote

.PHONY: format-check
format-check: ## Checks if format is correct
	@DUNE_CONFIG__GLOBAL_LOCK=disabled $(DUNE) build @fmt

.PHONY: create-switch
create-switch: ## Create opam switch
	opam switch create . 5.3.0 --deps-only --with-test -y

.PHONY: install
install: ## Install dependencies
	opam install . --deps-only --with-test --with-doc --with-dev-setup -y

.PHONY: pin
pin: ## Pin dependencies
	opam pin odoc-parser https://github.com/davesnx/odoc.git#markdown-types -y
	opam pin odoc https://github.com/davesnx/odoc.git#markdown-types -y

.PHONY: setup-githooks
setup-githooks: ## Setup githooks
	git config core.hooksPath .githooks

.PHONY: init
init: setup-githooks create-switch pin install ## Create a local dev enviroment

BENCH_FILES := $(wildcard bench/bench_*.ml)
BENCH_NAMES := $(patsubst bench/bench_%.ml,bench-%,$(BENCH_FILES))

.PHONY: bench
bench: $(BENCH_NAMES) ## Run all benchmarks

.PHONY: $(BENCH_NAMES)
$(BENCH_NAMES): bench-%: ## Run bench-% benchmark
	@$(DUNE) exec bench/bench_$*.exe --profile=release --display-separate-messages --no-print-directory

.PHONY: subst
subst: ## Run dune substitute
	$(DUNE) subst

.PHONY: docs
docs: ## Generate odoc documentation and markdown
	PARSEFF_GENERATE_MARKDOWN=$(PARSEFF_GENERATE_MARKDOWN) $(DUNE) build @doc @doc-markdown

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

.PHONY: website-preview
website-preview: ## Preview built website
	cd website && npm run preview
