package com.project.demo.rest.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import lombok.Data;

@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class PointOfSalesDto {
	@Nonnull
	private String id;
	@Nonnull
	private String name;
	private Boolean closed;
	@Nullable
	private WorkScheduleDto workScheduleDto;
}
