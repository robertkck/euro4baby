up: build deploy

deploy: build push deploy_remote

deploy_remote: 
	ssh robert@158.255.212.204 'docker pull rhawlik/euro4baby'
	ssh robert@158.255.212.204 'docker stop euro4baby || true && docker rm euro4baby || true'
	ssh robert@158.255.212.204 'docker run -p 3838:3838 -d --name euro4baby --restart unless-stopped rhawlik/euro4baby'

build: 
	docker build . -t euro4baby

run:
	docker run -p 3838:3838 -d --name euro4baby --restart unless-stopped euro4baby

push: 
	docker tag euro4baby rhawlik/euro4baby
	docker push rhawlik/euro4baby