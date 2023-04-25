package com.project.demo.persistence.repository;

import com.project.demo.persistence.PointOfSales;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;


import java.util.Set;

public interface PosRepositoryCustom {
	/**
	 * Gets all poses optionally filtered by parameters
	 *
	 * @param merchantId    - merchant id
	 * @param posName       - POS name
	 * @param operationArea - operation area name
	 * @param area          - POS area
	 * @param city          - POS city
	 * @param lang          - language
	 * @param pageable      - page request
	 * @return page of POSes
	 */
	@Nonnull
	Page<PointOfSales> getEnabledWorkingPosPage(@Nullable String merchantId,
	                                            @Nullable String posName,
	                                            @Nullable String operationArea,
	                                            @Nullable String area,
	                                            @Nullable String city,
	                                            @Nonnull String lang,
	                                            @Nonnull Pageable pageable);

	/**
	 * Gets all poses optionally filtered by parameters and active poses
	 *
	 * @param merchantId    - merchant id
	 * @param posName       - POS name
	 * @param operationArea - operation area name
	 * @param area          - POS area
	 * @param city          - POS city
	 * @param online        - online flag
	 * @param onlinePoses   - current online poses set
	 * @param lang          - language
	 * @param pageable      - page request
	 * @return page of POSes
	 */
	@Nonnull
	Page<PointOfSales> getEnabledWorkingPosPageWithOnline(@Nullable String merchantId,
	                                                      @Nullable String posName,
	                                                      @Nullable String operationArea,
	                                                      @Nullable String area,
	                                                      @Nullable String city,
	                                                      @Nonnull Boolean online,
	                                                      @Nonnull Set<String> onlinePoses,
	                                                      @Nonnull String lang,
	                                                      @Nonnull Pageable pageable);
}
