node {
    checkout scm
    def customImage = docker.build("build-sudoku-fsharp:${env.BUILD_ID}", "-f .jenkins/docker/Dockerfile .jenkins/docker")
    withCredentials([string(credentialsId: 'coverage-token', variable: 'COVERAGE_TOKEN')]) {
        customImage.inside('-v $HOME/.dotnet:/home/jenkins/.dotnet') {
            stage('Build') {
               sh 'dotnet restore'
            }
            stage('Test') {
               sh 'go test -coverprofile=coverage.txt -covermode=atomic'
            }
            stage('Upload coverage to codecov') {
               sh './scripts/codecov.sh -t $COVERAGE_TOKEN -K'
            }
        }
    }
}
