OSES = ['ubuntu/bionic64', 'debian/buster64']
BRANCHES = ['master', 'dev']

def linux_provision(branch)
	return <<-SHELL
	sudo apt-get update
	sudo apt-get -y install git # for debian, ubuntu has it
	sudo bash -c 'git clone https://github.com/ngs-lang/ngs.git && cd ngs && git checkout #{branch} && ./install.sh'
	SHELL
end

Vagrant.configure("2") do |config|
	OSES.each do |os|
		BRANCHES.each do |branch|
			config.vm.define os.split("/")[1] + '-' + branch do |c|
				c.vm.box = os
				c.vm.synced_folder ".", "/vagrant", disabled: true
				c.vm.provision "shell", inline: linux_provision(branch)
			end
		end
	end
end
