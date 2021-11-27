node {
    checkout scm
    def customImage = docker.build("build-sudoku-fsharp:${env.BUILD_ID}", "-f .jenkins/docker/Dockerfile .jenkins/docker")
    customImage.inside('-v $HOME/.dotnet:/home/jenkins/.dotnet') {
        stage('Install packages') {
           sh 'dotnet restore'
	}
	stage('Build dotnet') {
	    sh 'dotnet build'
	}
	stage('Test') {
	    sh 'dotnet test Sudoku.Test/Sudoku.Test.fsproj'
	}
    }
}
