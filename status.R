status<-function(datos){
        if (mode(datos) %in% c("logical", "numeric", "complex", "character")) {
                datos = data.frame(var = datos)
                input = "var"
        }
        status<-data.frame(
                Tipo = sapply(datos, function(x) class(x)), 
                Ceros=paste0(sapply(datos, function(x) sum(x==0, na.rm = T)),paste0(" (",round(sapply(datos, function(x) sum(x == 0, na.rm = T))*100/nrow(datos), 2),"%)")),
                Nulos=paste0(sapply(datos, function(x) sum(is.null(x))),paste0(" (",round(sapply(datos, function(x) sum(is.null(x)))*100/nrow(datos), 2),"%)")),
                Vacios=paste0(sapply(datos, function(x) sum(is.na(x))),paste0(" (",round(sapply(datos, function(x) sum(is.na(x)))*100/nrow(datos), 2),"%)")),
                Infi = paste0(sapply(datos, function(x) sum(is.infinite(x))),paste0(" (",round(sapply(datos, function(x) sum(is.infinite(x)))*100/nrow(datos), 2),"%)")),
                Error = paste0(sapply(datos, function(x) sum(is.nan(x))),paste0(" (",round(sapply(datos, function(x) sum(is.infinite(x)))*100/nrow(datos), 2),"%)")),
                Unicos=sapply(datos,function(x) sum(!is.na(unique(x))))
        )
        return(status)
}