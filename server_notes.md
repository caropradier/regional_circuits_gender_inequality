# I create  a folder with different configurations of conf files for exploration.

To test them, we need to rename them shiny-server.conf and save them in /etc/shiny-server/shiny-server.conf

with `sudo cp shiny-server.conf /etc/shiny-server/shiny-server.conf`

To test, always restart shiny server to check the new config with

`sudo systemctl restart shiny-server`

# Updating the app:

1. Connect to the udem VPN

2. VS code: connect tu UDEM_VM (>< symbol on bottom left corner) and open terminal (ctrl + ~)

3. Check location using `ll`

4. Move to git folder using `cd GIT`

5. Move to latam folder using `cd regional_circuits_gender_inequality`

6. `git pull` to bring latest modifications

7. Copy elements inside the latam_app folder to /srv/dev/latam_app/ using:
`sudo cp -r regional_circuits_gender_inequality/app/* /srv/dev/latam_app/`
(password needed)

8. Done! check webpage :) 

Notes: Can't remember a commend? reverse-i-search