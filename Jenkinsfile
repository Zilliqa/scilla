def podDefinition = """
apiVersion: "v1"
kind: "Pod"
spec:
  nodeSelector:
    beta.kubernetes.io/os: "linux"
  containers:
  - name: "ubuntu"
    image: "648273915458.dkr.ecr.us-west-2.amazonaws.com/scilla:429e2f9"
    command:
    - cat
    tty: true
    resources:
      requests:
        ephemeral-storage: "20Gi"
      limits:
        ephemeral-storage: "20Gi"
"""

String skipciMessage = 'Aborting because commit message contains [skip ci]'

timestamps {
  ansiColor('gnome-terminal') {
    podTemplate(yaml: podDefinition) {
      timeout(time: 60, unit: 'MINUTES') {
        node(POD_LABEL) {
          try {
            stage('Checkout scm') {
                checkout scm
                def pr_skipci = "0"
                try {
                  if (env.CHANGE_TITLE != null && env.CHANGE_TITLE != "") {
                    pr_skipci = sh(script: "echo ${env.CHANGE_TITLE.replace("(","").replace(")","")} | fgrep -ie '[skip ci]' -e '[ci skip]' | wc -l", returnStdout: true).trim()
                  }
                } catch (err) {
                  println err.getMessage()
                  error("Error reading the Pull Request title, please check and eventually remove special characters")
                }
                def skipci = sh(script: "git log -1 --pretty=%B | fgrep -ie '[skip ci]' -e '[ci skip]' | wc -l", returnStdout: true).trim()
                if (skipci != "0" || pr_skipci != "0") {
                  error(skipciMessage)
                }
            }
            container('ubuntu') {
                env.VCPKG_ROOT="/vcpkg"
                env.SCILLA_REPO_ROOT="/scilla/0"
                stage('Update source code') {
                  sh "mkdir -p /scilla/0 && cp -r ${WORKSPACE}/* /scilla/0 && ln -s ${WORKSPACE}/.git /scilla/0/.git"
                  sh "cd /scilla/0 && eval \$(opam env) && LD_LIBRARY_PATH=/scilla/0/vcpkg_installed/x64-linux-dynamic/lib opam install reason.3.8.2 --yes"
                  sh "apt update && apt install -y sudo"
                  sh "./scripts/install_shellcheck_ubuntu.sh"
                }
                stage('Test') {
                  sh "cd /scilla/0 && eval \$(opam env) && LD_LIBRARY_PATH=/scilla/0/vcpkg_installed/x64-linux-dynamic/lib make test"
                }
                stage('Test server') {
                  sh "cd /scilla/0 && eval \$(opam env) && LD_LIBRARY_PATH=/scilla/0/vcpkg_installed/x64-linux-dynamic/lib make test_server"
                }
                stage('Coveralls') {
                  sh "cd /scilla/0 && eval \$(opam env) && LD_LIBRARY_PATH=/scilla/0/vcpkg_installed/x64-linux-dynamic/lib make coveralls TRAVIS_JOB_ID=${BUILD_NUMBER}"
                }
                stage('Lint') {
                  sh "cd /scilla/0 && eval \$(opam env) && LD_LIBRARY_PATH=/scilla/0/vcpkg_installed/x64-linux-dynamic/lib make lint"
                }
            }
          } catch (err) {
            if (err.getMessage() == skipciMessage)
              currentBuild.result = 'SUCCESS'
            else
              throw err
          }
        }
      }
    }
  }
}
