library(tidyverse)
library(gdalUtils)
library(pbapply)

begin_date <- as.Date("2003/01/01", "%Y/%m/%d")
end_date <- as.Date("2004/12/31", "%Y/%m/%d")

date_seq <- seq(from=begin_date, to=end_date, by="day")



for (d in 1:length(date_seq)){
    day <- date_seq[d]
    year <- format(day, "%Y")
    date <- format(day, "%Y.%m.%d")
    baseURL <-  paste0("https://acdisc.gesdisc.eosdis.nasa.gov/",
                       "data/Aqua_AIRS_Level3/AIRS3STD.006/")

    t <- system(paste0("wget -q -nH -nd ",
                       baseURL, year,
                       ' -O - | grep "',
                       date, '" | cut -f4 -d\\" '), intern=T)

    file <- paste0(baseURL, year, "/", t[1])

    system(paste0("wget --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --auth-no-challenge=on --keep-session-cookies --content-disposition ", file))

    if (t[1] %in% dir()){
        subs <- get_subdatasets(t[1])

        gdal_translate(subs[19], dst_dataset = "ascend_tmp.tif")
        gdal_translate(subs[197], dst_dataset = "descend_tmp.tif")

        r.a <- stack("ascend_tmp.tif")
        r.d <- stack("descend_tmp.tif")

        df.a <- as.data.frame(r.a) %>%
            mutate(id = row_number()) %>%
            pivot_longer(-id, names_to="layer", values_to="temp") %>%
            filter(!is.na(temp)) %>%
            group_by(id) %>%
            mutate(temp_diff = temp - lead(temp),
                   inversion = ifelse(temp_diff < 0, 1, 0)) %>%
            filter(row_number() == 1) %>%
            dplyr::select(id, inversion) %>%
            right_join(data.frame(id = seq(1, dim(r.a)[1]*dim(r.a)[2])))

        df.d <- as.data.frame(r.d) %>%
            mutate(id = row_number()) %>%
            pivot_longer(-id, names_to="layer", values_to="temp") %>%
            filter(!is.na(temp)) %>%
            group_by(id) %>%
            mutate(temp_diff = temp - lead(temp),
                   inversion = ifelse(temp_diff < 0, 1, 0)) %>%
            filter(row_number() == 1) %>%
            dplyr::select(id, inversion) %>%
            right_join(data.frame(id = seq(1, dim(r.d)[1]*dim(r.d)[2])))

        rinv.a <- setValues(raster(r.a, 1), values=df.a$inversion)
        rinv.d <- setValues(raster(r.d, 1), values=df.d$inversion)

        writeRaster(rinv.a,
                    filename=paste0("tiff/",
                                    day,
                                    "_ascend.tif"),
                    format="GTiff")
        writeRaster(rinv.d,
                    filename=paste0("tiff/",
                                    day,
                                    "_descend.tif"),
                    format="GTiff")
        system(paste0("rm *tmp.tif* ", t[1]))

        gc()
        print(date)
    } else {
        print(paste0("No file for ", date))
        system(paste0("echo ", date, " >> missing.txt"))
    }
}
    

    
