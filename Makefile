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

.PHONY: bench
bench: ## Run all benchmarks
	@$(DUNE) exec bench/bench_micro.exe --profile=release --display-separate-messages --no-print-directory
	@$(DUNE) exec bench/bench_vs_angstrom.exe --profile=release --display-separate-messages --no-print-directory
	@$(DUNE) exec bench/bench_json.exe --profile=release --display-separate-messages --no-print-directory
	@$(DUNE) exec bench/bench_csv.exe --profile=release --display-separate-messages --no-print-directory
	@$(DUNE) exec bench/bench_arithmetic.exe --profile=release --display-separate-messages --no-print-directory

.PHONY: bench-micro
bench-micro: ## Run micro benchmarks
	@$(DUNE) exec bench/bench_micro.exe --profile=release --display-separate-messages --no-print-directory

.PHONY: bench-vs-angstrom
bench-vs-angstrom: ## Run benchmarks vs Angstrom
	@$(DUNE) exec bench/bench_vs_angstrom.exe --profile=release --display-separate-messages --no-print-directory

.PHONY: bench-json
bench-json: ## Run JSON benchmark (all parsers)
	@$(DUNE) exec bench/bench_json.exe --profile=release --display-separate-messages --no-print-directory

.PHONY: bench-csv
bench-csv: ## Run CSV benchmark (all parsers)
	@$(DUNE) exec bench/bench_csv.exe --profile=release --display-separate-messages --no-print-directory

.PHONY: bench-arithmetic
bench-arithmetic: ## Run arithmetic benchmark (all parsers)
	@$(DUNE) exec bench/bench_arithmetic.exe --profile=release --display-separate-messages --no-print-directory

.PHONY: bench-binary
bench-binary: ## Run binary parsing benchmarks
	@$(DUNE) exec bench/bench_binary.exe --profile=release --display-separate-messages --no-print-directory

.PHONY: bench-utf8
bench-utf8: ## Run UTF-8 parsing benchmarks
	@$(DUNE) exec bench/bench_utf8.exe --profile=release --display-separate-messages --no-print-directory

.PHONY: subst
subst: ## Run dune substitute
	$(DUNE) subst

.PHONY: docs
docs: ## Generate odoc documentation and markdown
	PARSEFF_GENERATE_MARKDOWN=$(PARSEFF_GENERATE_MARKDOWN) $(DUNE) build @doc @doc-markdown

.PHONY: website-dev
website-dev: docs ## Run website dev server (generated from .mld)
	cd website && npm run dev

.PHONY: website-build
website-build: docs ## Build website (generates markdown from .mld then builds Astro)
	@if [ ! -d website/node_modules ]; then \
		echo "Installing website dependencies..."; \
		cd website && npm ci; \
	fi
	cd website && npm run build

.PHONY: website-preview
website-preview: ## Preview built website
	cd website && npm run preview
