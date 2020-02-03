# MiMo

To try out the app, please visit [https://nickriches.shinyapps.io/MiMo/](https://nickriches.shinyapps.io/MiMo/). There are full instructions included in the app. It should only take about ten minutes to do some basic analysis.

Please note that the app is running on my personal ShinyApps account, and if there is a lot of demand for the app, it may cease to function at this web address. Apologies in advance for this - I do not currently have the resources to pay for the full professional account.

By far the best way to run the app is to install it to your own server...

**Installing to a shinyapps.io server**

This GitHub site contains the code for the MiMo language app. To get this up and running on a shinyapps.io server, here's what you need to do...

(1) Install [R](https://www.r-project.org/) and [RStudio](https://rstudio.com/products/rstudio/download/) on a computer

(2) Join [Shiny Apps](https://www.shinyapps.io/) and set up a Free Account.

(3) Create a Shiny Apps Project in RStudio ``New Project > New Directory > Create Shiny Web Application > Create Project``

(4) Download the above files into your Shiny Apps directory (using the green "Download" button), making sure you write over any existing files with the same name.

(5) Upload your App to Shiny Apps (using the blue symbol at the top right hand corner of your screen)

(6) When the upload is finished (it can take around 10 minutes), your default browser should automatically open the web app.

**Installing to a different ShinyApps server**

The IT service at your place of work may be able to set up a ShinyApps server specifically for you to run your app. (They have very kindly helped me to do this at my own institution, Newcastle University).

**Running the app locally**

You may also choose to run your app locally on PC, Mac or Linux computer, without installing to a server. To do this, (a) install R and RStudio, (b) make sure that all of the files in the .www folder are copied and pasted into the main folder, (c) select all of the text in the app.R file, and run.

**Further information**

For more resources on how to use Shiny Apps go to [https://shiny.rstudio.com/tutorial/](https://shiny.rstudio.com/tutorial/)

**Feedback**

If you have any questions or queries, please obtain a GitHub account, and raise an issue using the ``Issue`` tab above. I am open to suggestions on how to improve and develop the app (for example, by adding adequate morphological parsing for languages other than English), but please note that the amount of time I can dedicate to this is limited. Please also note that I am a relative R newbie, and there is a lot of room for refinement of the source code.

**Acknowledgements**

Full acknowledgements are provided in the app itself. However, I should briefly mention **Newcastle University**, for supporting me in the development of this app, the **RStudio consortium** for creating the fantastic ShinyApps framework, and **Brian MacWhinney's CLaN software (developed with Leonid Spektor)** and the **Talkbank** repository, for providing normative data.
