package com.project.demo.rest.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;
import jakarta.annotation.Nonnull;
import lombok.Data;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@JsonInclude(JsonInclude.Include.NON_NULL)
@Data
public class WorkScheduleDto {
	@Nonnull
	private List<TimeScheduleDto> sun = new ArrayList();
	@Nonnull

	private List<TimeScheduleDto> mon = new ArrayList();
	@Nonnull
	private List<TimeScheduleDto> tue = new ArrayList();
	@Nonnull
	private List<TimeScheduleDto> wed = new ArrayList();
	@Nonnull
	private List<TimeScheduleDto> thu = new ArrayList();
	@Nonnull
	private List<TimeScheduleDto> fri = new ArrayList();
	@Nonnull
	private List<TimeScheduleDto> sat = new ArrayList();
	private Date closedUntil;
}
