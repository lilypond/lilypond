Ansible Playbooks
-----------------

This directory contains [Ansible](https://www.ansible.com/) playbooks to describe the setup for building LilyPond binaries.

For example, to set up a VM with Alma Linux 8 to build binaries for Linux:
1. Download one of the [official images for Alma Linux 8](https://almalinux.org/get-almalinux/).
2. Choose a "Minimal Install", set a `root` password, and create an additional account (for example `lily`).
3. After installation, set up password-less `ssh` at least for the `root` account.
4. [Install Ansible](https://docs.ansible.com/ansible/latest/installation_guide/index.html) and create an inventory.
   The easiest way is to create a file called `inventory.ini` in this directory, see below.
5. Run `ansible-playbook -i inventory.ini alma8.yml` to install all needed packages.
   Run again to reflect changes to the playbook, to update the system, or just to ensure that everything is properly installed.
6. Especially after the first run, or whenever a new kernel was installed, reboot the system.
   For good measure, just always reboot when Ansible shows that something has changed.

An exmple `inventory.ini` could look like this:
```
[lily_alma8]
root@Lily-AlmaLinux8

[lily_ubuntu22_04]
root@Lily-Ubuntu22.04
```

The lines with square brackets, such as `[lily_alma8]`, set the group name and **must not be changed**.
The following line gives the host passed to `ssh`.
Make sure that this works without password and logs into the `root` account (in order to install packages).
