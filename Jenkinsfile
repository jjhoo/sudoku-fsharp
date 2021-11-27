node {
    checkout scm
    def customImage = docker.build("build-sudoku-fsharp:${env.BUILD_ID}", "-f .jenkins/docker/Dockerfile .jenkins/docker")

    withCredentials([string(credentialsId: 'coverage-token', variable: 'COVERAGE_TOKEN')]) {
        customImage.inside('-v $HOME/.dotnet:/home/jenkins/.dotnet') {
            stage('Install packages') {
	        sh 'dotnet restore'
	    }
	    stage('Build dotnet') {
	        sh 'dotnet build'
	    }
	    stage('Test') {
	        sh 'dotnet test --collect:"XPlat Code Coverage" Sudoku.Test/Sudoku.Test.fsproj'
	    }
            stage('Upload coverage to codecov') {
	        sh '~/.local/bin/codecov --token $COVERAGE_TOKEN --no-color'
            }
        }
    }
}
