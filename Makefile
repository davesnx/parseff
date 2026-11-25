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
test: ## Run the unit tests
	$(DUNE) build --root . @runtest

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
create-switch: ## Create opam switch
	opam switch create . 5.5.2 --deps-only --with-test -y

.PHONY: install
install: ## Install dependencies
	opam install . --deps-only --with-test --with-doc --with-dev-setup -y

.PHONY: init
init: create-switch install ## Create a local dev enviroment

OXCAML_SWITCH ?= parseff-oxcaml-ox9
OXCAML_COMPILER ?= ocaml-variants.5.4.0+ox,oxcaml-compiler.5.4.0-ox9
OXCAML_DUNE ?= opam exec --switch=$(OXCAML_SWITCH) -- dune
OXCAML_BUILD_DIR ?= _build-oxcaml

.PHONY: create-switch-oxcaml
create-switch-oxcaml: ## Create the OxCaml opam switch
	opam switch create $(OXCAML_SWITCH) --packages=$(OXCAML_COMPILER) --repos ox=git+https://github.com/oxcaml/opam-repository.git,default -y

.PHONY: install-oxcaml
install-oxcaml: ## Install the test dependencies into the OxCaml switch
	opam install --switch=$(OXCAML_SWITCH) dune uucp alcotest -y

.PHONY: test-oxcaml
test-oxcaml: ## Run the unit tests with OxCaml, and fail on a safety alert in lib/
	@out=$$(DUNE_CACHE=disabled $(OXCAML_DUNE) build --root . --build-dir=$(OXCAML_BUILD_DIR) ./lib/parseff.cmxa 2>&1); status=$$?; \
	if [ -n "$$out" ]; then printf '%s\n' "$$out"; fi; \
	if [ $$status -ne 0 ]; then exit $$status; fi; \
	if printf '%s\n' "$$out" | grep -q '^Alert'; then echo "lib/ has OxCaml alerts"; exit 1; fi
	$(OXCAML_DUNE) build --root . --build-dir=$(OXCAML_BUILD_DIR) @test/runtest

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
