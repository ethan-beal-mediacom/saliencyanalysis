#' Produce Salience heatmap from Raw MSI Net output
#'
#' @param saliency_files_directory path to directory where raw MSI Net output is found
#' @param raw_image_directory path to directory where raw creatives are found
#' @param output_directory path to directory you want the heatmaps outputed into
#' @param output_only logical. if `TRUE` all other edits are deleted and only final heatmap is kept
#'
#' @return jpeg where heatmap is overlayed original creative
#' @export
#'
#' @examples
saliency_heatmap <- function(saliency_files_directory, raw_image_directory, output_directory, output_only = FALSE){

  ####### loop

  saliency_files = list.files(saliency_files_directory, full.names =TRUE)
  raw_image_files = list.files(raw_image_directory, full.names =TRUE)

  imagenames = list.files(saliency_files_directory, full.names =FALSE)

  if(length(saliency_files) > 0 & length(raw_image_files) > 0){

    lapply(1:length(saliency_files), function(i){

      ### read files as rasters
      ## saliency image - SI
      filenameSI<- saliency_files[i]
      imagename <- imagenames[i]  # identify image name for later use
      rSI <- raster::raster(filenameSI) # read in file
      rSI_spdf <- methods::as(rSI, "SpatialPixelsDataFrame")
      rSI_df <- as.data.frame(rSI_spdf) # make into df
      colnames(rSI_df) <- c("value", "x", "y")


      ## background image - BI
      filenameBI <- raw_image_files[i]
      rBI <- raster::stack(filenameBI) # as it is in colour read it in as a stack
      #plotRGB(rBI,r=1,g=2,b=3, stretch = "lin") # plot for sanity check
      rBI_spdf <- methods::as(rBI, "SpatialPixelsDataFrame")
      rBI_df <- as.data.frame(rBI_spdf)
      # remove alpha column if there is one
      if(ncol(rBI_df) == 6){
        rBI_df = rBI_df[,-4]
      }
      colnames(rBI_df) <- c("band1", "band2", "band3", "x", "y")
      rBI_df <- rBI_df %>% dplyr::mutate(bw = (band1 + band2 + band3)/3)


      ## make plots

      # overlay
      my_palette = rev(c(RColorBrewer::brewer.pal(11, "RdGy")[1], RColorBrewer::brewer.pal(11, "Spectral")[c(1:9)], RColorBrewer::brewer.pal(9, "Blues")[c(5, 4, 3, 2, 1)]))
      # filled contor lines
      overlay <- ggplot2::ggplot(data=rSI_df, ggplot2::aes(x=x, y=y, z=value)) +
        ggplot2::geom_tile(ggplot2::aes(fill = value)) +
        ggplot2::stat_contour(geom = "polygon", ggplot2::aes(fill = ..level..), bins = 15) +
        ggplot2::geom_contour(data=rSI_df, ggplot2::aes(x=x, y=y, z=value), bins =7, colour = "black", size = 0.3) + # contour lines
        ggplot2::scale_fill_gradientn(colours = my_palette) +
        ggplot2::theme(axis.line=ggplot2::element_blank(),axis.text.x=ggplot2::element_blank(),
                       axis.text.y=ggplot2::element_blank(),axis.ticks=ggplot2::element_blank(),
                       axis.title.x=ggplot2::element_blank(),
                       axis.title.y=ggplot2::element_blank(),legend.position="none",
                       panel.background=ggplot2::element_blank(),panel.border=ggplot2::element_blank(),panel.grid.major=ggplot2::element_blank(),
                       panel.grid.minor=ggplot2::element_blank(),plot.background=ggplot2::element_blank())

      # base
      base <- ggplot2::ggplot() +
        ggplot2::geom_tile(data=rBI_df, ggplot2::aes(x=x, y=y, fill=bw), alpha=1) +
        ggplot2::scale_fill_gradientn(colours = grey(seq(0, 1, length = 16))) +
        ggplot2::theme(axis.line=ggplot2::element_blank(),axis.text.x=ggplot2::element_blank(),
                       axis.text.y=ggplot2::element_blank(),axis.ticks=ggplot2::element_blank(),
                       axis.title.x=ggplot2::element_blank(),
                       axis.title.y=ggplot2::element_blank(),legend.position="none",
                       panel.background=ggplot2::element_blank(),panel.border=ggplot2::element_blank(),panel.grid.major=ggplot2::element_blank(),
                       panel.grid.minor=ggplot2::element_blank(),plot.background=ggplot2::element_blank())


      ## save plots
      # 1 - raw
      step_name = "/1_raw/"
      if (!dir.exists(paste0(output_directory, step_name))){# make directory if it dosent exist
        dir.create(paste0(output_directory, step_name))
      }
      # overlay
      if (!dir.exists(paste0(output_directory, step_name, "overlay"))){# make directory if it dosent exist
        dir.create(paste0(output_directory, step_name, "overlay"))
      }
      overlay_filename_1_raw <- paste(output_directory, step_name, "overlay/", imagename, sep = "")
      ggplot2::ggsave(filename = overlay_filename_1_raw , plot = overlay)
      # base
      if (!dir.exists(paste0(output_directory, step_name, "base"))){# make directory if it dosent exist
        dir.create(paste0(output_directory, step_name, "base"))
      }
      base_filename_1_raw <- paste(output_directory, step_name, "base/", imagename, sep = "")
      ggplot2::ggsave(filename = base_filename_1_raw , plot = base)



      # 2 - resized - resize plot using magick
      step_name = "/2_resize/"
      if (!dir.exists(paste0(output_directory, step_name))){# make directory if it dosent exist
        dir.create(paste0(output_directory, step_name))
      }

      # overlay
      if (!dir.exists(paste0(output_directory, step_name, "overlay"))){# make directory if it dosent exist
        dir.create(paste0(output_directory, step_name, "overlay"))
      }
      overlay_filename_2_resize <- paste(output_directory, step_name, "overlay/", imagename, sep = "")
      magick::image_read(overlay_filename_1_raw) %>% # read in raw
        magick::image_trim() %>%  # remove border created by ggsave
        magick::image_scale(paste(rSI@extent@xmax, "x", rSI@extent@ymax, "!", sep ="")) %>%  # scale image to correct dimensions
        magick::image_write(path = overlay_filename_2_resize, format = "png") # save again as resized
      # base
      if (!dir.exists(paste0(output_directory, step_name, "base"))){# make directory if it dosent exist
        dir.create(paste0(output_directory, step_name, "base"))
      }
      base_filename_2_resize <- paste(output_directory, step_name, "base/", imagename, sep = "")
      magick::image_read(base_filename_1_raw) %>% # read in raw
        magick::image_trim() %>%  # remove border created by ggsave
        magick::image_scale(paste(rBI@extent@xmax, "x", rBI@extent@ymax, "!", sep ="")) %>%  # scale image to correct dimensions
        magick::image_write(path = base_filename_2_resize, format = "png") # save again as resized


      ## edit the plots

      # fade the saliency overlay
      overlay_png <- png::readPNG(overlay_filename_2_resize) # reback in image
      # if there is not a fourth (alpha) channel add one
      if(dim(overlay_png)[3] == 3){
        overlay_png <- abind::abind(overlay_png, overlay_png[,,1])
      }
      # set alpha (transparency)
      overlay_png[,,4] = 0.4

      # 3 - fade
      step_name = "/3_fade/"
      if (!dir.exists(paste0(output_directory, step_name))){# make directory if it dosent exist
        dir.create(paste0(output_directory, step_name))
      }
      overlay_filename_3_fade <- paste(output_directory, step_name, imagename, sep = "")
      png::writePNG(overlay_png, overlay_filename_3_fade) # write again

      # read back both images
      overlay_png <- magick::image_read(overlay_filename_3_fade)
      base_png <- magick::image_read(base_filename_2_resize)

      output_img <- magick::image_composite(base_png, overlay_png)

      # 4 - final output
      step_name = "/4_final_output/"
      if (!dir.exists(paste0(output_directory, step_name))){# make directory if it dosent exist
        dir.create(paste0(output_directory, step_name))
      }
      output_filename <- paste(output_directory, step_name, imagename, sep = "")
      magick::image_write(output_img, path = output_filename, format = "png")


    })

    if(output_only == TRUE){
      unlink(paste0(output_directory, "/1_raw"), recursive = TRUE)
      unlink(paste0(output_directory, "/2_resize"), recursive = TRUE)
      unlink(paste0(output_directory, "/3_fade"), recursive = TRUE)
    }
  }else{
    print("error accessing saliency or raw image files")
  }


}
