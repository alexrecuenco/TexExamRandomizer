do_package_checks()

LinuxSetup <- R6::R6Class(
    "LinuxSetup", inherit = TicStep,

    public = list(
        run = function() {
            if (Sys.getenv("TRAVIS_OS_NAME") == "linux") {
                dir.create("~/.R", recursive = TRUE)
                file.create("~/.R/Makevars")

                TEXT = "
                VER=-4.9
                CC=gcc$(VER) -std=c11
                CXX=g++$(VER)
                SHLIB_CXXLD=g++$(VER)
                "

                write(TEXT, file = "~/.R/Makevars", append = FALSE)
                print(TEXT)

            }
        }
    )
)

step_setup_linux_gcc49 <- function() {
    LinuxSetup$new()
}

# VkVSPS00LjkKQ0M9Z2NjJChWRVIpIC1zdGQ9YzExIApDWFg9ZysrJChWRVIpClNITElCX0NYWExEPWcrKyQoVkVSKQpGQz1nZm9ydHJhbgpGNzc9Z2ZvcnRyYW4K, The base 64 string is
#VER=-4.9
#CC=gcc$(VER) -std=c11
#CXX=g++$(VER)
#SHLIB_CXXLD=g++$(VER)
#FC=gfortran
#F77=gfortran
# - if [[ "$TRAVIS_OS_NAME" == "linux" ]]; then sudo update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.9 100; fi
# - if [[ "$TRAVIS_OS_NAME" == "linux" ]]; then mkdir -p ~/.R; fi
# - if [[ "$TRAVIS_OS_NAME" == "linux" ]]; then echo "VkVSPS00LjkKQ0M9Z2NjJChWRVIpIC1zdGQ9YzExIApDWFg9ZysrJChWRVIpClNITElCX0NYWExEPWcrKyQoVkVSKQpGQz1nZm9ydHJhbgpGNzc9Z2ZvcnRyYW4K" | base64 --decode > ~/.R/Makevars; fi
# - if [[ "$TRAVIS_OS_NAME" == "linux" ]]; then cat ~/.R/Makevars; fi

get_stage("before_install") %>%
    add_step(step_setup_linux_gcc49())


if (Sys.getenv("id_rsa") != "" && !ci()$is_tag()) {
  # pkgdown documentation can be built optionally. Other example criteria:
  # - `inherits(ci(), "TravisCI")`: Only for Travis CI
  # - `Sys.getenv("BUILD_PKGDOWN") != ""`: If the env var "BUILD_PKGDOWN" is set
  # - `Sys.getenv("TRAVIS_EVENT_TYPE") == "cron"`: Only for Travis cron jobs
  get_stage("before_deploy") %>%
    add_step(step_setup_ssh())

  get_stage("deploy") %>%
    add_step(step_build_pkgdown()) %>%
    add_step(step_push_deploy(path = "docs", branch = "gh-pages"))
}
