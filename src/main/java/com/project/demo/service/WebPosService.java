package com.project.demo.service;

import com.project.demo.persistence.PointOfSales;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.Set;

public interface WebPosService {

	/**
	 * Gets all poses optionally filtered by POS name, city, area, operation area
	 *
	 * @param merchantId    - merchant's identifier
	 * @param posName       - POS name
	 * @param operationArea - operation area name
	 * @param area          - POS area
	 * @param city          - POS city
	 * @param lang          - language
	 * @param online        - online flag
	 * @param onlinePoses   - current online poses set
	 * @param pageable      - page request
	 * @return page of POSes
	 */
	@Nonnull
	Page<PointOfSales> getEnabledWorkingPosPage(@Nullable String merchantId,
	                                            @Nullable String posName,
	                                            @Nullable String operationArea,
	                                            @Nullable String area,
	                                            @Nullable String city,
	                                            @Nullable Boolean online,
	                                            @Nonnull Set<String> onlinePoses,
	                                            @Nonnull String lang,
	                                            @Nonnull Pageable pageable);
}
