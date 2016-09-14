NPMD <- (Et[length(Et)] - Vt[1]) / MD(Et)

Burke <- (Et[length(Et)] - Vt[1]) /
          sqrt((1/length(Et)) * sum(MD(Et, n = round(length(Et) / 20))^2))
