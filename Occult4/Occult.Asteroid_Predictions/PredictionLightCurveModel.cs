using System;
using System.IO;

namespace Occult.Asteroid_Predictions
{
	internal class PredictionLightCurveModel
	{
		internal const float AsteroidPlusStarRadius = 85f;

		private const int MaxArrayValue = 200;

		private const int ArrayHalfWidth = 100;

		private static int[,] StarArray = new int[201, 201];

		private static bool[,] AsteroidUnrotatedArray = new bool[201, 201];

		private static bool[,] AsteroidArray = new bool[201, 201];

		private static bool Rectangle = false;

		private static int CenterX = 100;

		private static int CenterY = 100;

		internal static double StarRadius_mas = 1.0;

		internal static double SemiMajorAxis_mas = 1.0;

		internal static double SemiMinorAxis_mas = 1.0;

		internal static double SemiMajor_mas_Sqr = 1.0;

		private static int StarRadiusRange = 1;

		private static int AsteroidRadiusRange = 1;

		internal static double PA_MajorAxis = 0.0;

		internal static double CosPA = 1.0;

		internal static double SinPA = 0.0;

		internal static double LimbDarkening = 1.0;

		internal static double PAofPathDirection = 90.0;

		internal static double Scale_masPerUnit = 1.0;

		internal static int AsteroidOffsetX = 0;

		internal static int AsteroidOffsetY = 0;

		internal static int FullIllumination = 10;

		internal static int TotalStarIllumination = 0;

		internal static int TotalAsteroidArea = 0;

		internal static double AxisRatio = 0.0;

		internal static float BrightnessRatio_Asteroid_over_Star = 0f;

		internal double LightCurveScale_arcsec => Scale_masPerUnit;

		internal void CreateLightCurveArrays(double StarDia_mas, double Limb_Darkening, double AsteroidMajorAxis_mas, double AsteroidMinorAxis_mas, double AsteroidPA_Major_Deg, bool rectangle)
		{
			Rectangle = rectangle;
			_ = new float[201];
			SetDiameters(StarDia_mas, Limb_Darkening, AsteroidMajorAxis_mas, AsteroidMinorAxis_mas, AsteroidPA_Major_Deg);
			CreateStarArray();
			CreateUnrotatedAsteroidArray(Rectangle);
			CreateAsteroidArray();
		}

		internal void SetDiameters(double StarDia_mas, double Limb_Darkening, double AsteroidMajorAxis_mas, double AsteroidMinorAxis_mas, double AsteroidPA_Major_Deg)
		{
			StarRadius_mas = StarDia_mas / 2.0;
			SemiMajorAxis_mas = AsteroidMajorAxis_mas / 2.0;
			SemiMajor_mas_Sqr = SemiMajorAxis_mas * SemiMajorAxis_mas;
			SemiMinorAxis_mas = AsteroidMinorAxis_mas / 2.0;
			AxisRatio = SemiMinorAxis_mas / SemiMajorAxis_mas;
			PA_MajorAxis = AsteroidPA_Major_Deg / (180.0 / Math.PI);
			CosPA = Math.Cos(PA_MajorAxis);
			SinPA = Math.Sin(PA_MajorAxis);
			LimbDarkening = Limb_Darkening;
			Scale_masPerUnit = (SemiMajorAxis_mas + StarRadius_mas) / 85.0;
		}

		internal static int GetStar(int x, int y)
		{
			return StarArray[x + CenterX, y + CenterY];
		}

		internal static void SetStar(int value, int x, int y)
		{
			StarArray[x + CenterX, y + CenterY] = value;
		}

		internal static void CreateStarArray()
		{
			StarArray = new int[201, 201];
			StarRadiusRange = Convert.ToInt32(StarRadius_mas / Scale_masPerUnit);
			TotalStarIllumination = 0;
			for (int i = -StarRadiusRange; i <= 0; i++)
			{
				for (int j = -StarRadiusRange; j <= 0; j++)
				{
					int num = StarBrightness(Math.Sqrt(i * i + j * j) * Scale_masPerUnit, StarRadius_mas, LimbDarkening);
					_ = num != 10 && num != 0;
					SetStar(num, i, j);
					SetStar(num, -i, j);
					SetStar(num, i, -j);
					SetStar(num, -i, -j);
				}
			}
			for (int k = -StarRadiusRange; k <= StarRadiusRange; k++)
			{
				for (int l = -StarRadiusRange; l <= StarRadiusRange; l++)
				{
					TotalStarIllumination += GetStar(k, l);
				}
			}
		}

		private static int StarBrightness(double RadialDistance_mas, double StarRadius_mas, double LimbDarkeningCoefficient)
		{
			if (RadialDistance_mas >= StarRadius_mas)
			{
				return 0;
			}
			return Convert.ToInt32((double)FullIllumination * (1.0 - LimbDarkeningCoefficient * (1.0 - Math.Sqrt(1.0 - RadialDistance_mas * RadialDistance_mas / StarRadius_mas / StarRadius_mas))));
		}

		internal static bool GetAsteroid(int x_fromCenter, int y_fromCenter)
		{
			if (x_fromCenter < -100 || x_fromCenter > 100 || y_fromCenter < -100 || y_fromCenter > 100)
			{
				return false;
			}
			return AsteroidArray[x_fromCenter + CenterX, y_fromCenter + CenterY];
		}

		internal static bool SetAsteroidUnrotated(bool value, int x_fromCenter, int y_fromCenter)
		{
			if (x_fromCenter < -100 || x_fromCenter > 100 || y_fromCenter < -100 || y_fromCenter > 100)
			{
				return false;
			}
			AsteroidUnrotatedArray[x_fromCenter + CenterX, y_fromCenter + CenterY] = value;
			return true;
		}

		internal static void CreateUnrotatedAsteroidArray(bool Rectangle)
		{
			AsteroidUnrotatedArray = new bool[201, 201];
			AsteroidRadiusRange = Convert.ToInt32(SemiMajorAxis_mas / Scale_masPerUnit);
			if (Rectangle)
			{
				AsteroidRadiusRange = Convert.ToInt32(Math.Sqrt(SemiMajorAxis_mas * SemiMajorAxis_mas + SemiMinorAxis_mas * SemiMinorAxis_mas) / Scale_masPerUnit);
			}
			for (int i = 0; i <= 100; i++)
			{
				double num = (double)i * Scale_masPerUnit;
				if (num < SemiMajorAxis_mas)
				{
					double num2;
					if (Rectangle)
					{
						num2 = SemiMinorAxis_mas;
					}
					else
					{
						double num3 = num * num;
						num2 = Math.Sqrt(SemiMajor_mas_Sqr - num3) * AxisRatio;
					}
					int num4 = Convert.ToInt32(num2 / Scale_masPerUnit);
					for (int j = -num4; j <= num4; j++)
					{
						SetAsteroidUnrotated(value: true, j, i);
						SetAsteroidUnrotated(value: true, j, -i);
					}
				}
			}
			if (!Utilities.IsInVisualStudio)
			{
				return;
			}
			TotalAsteroidArea = 0;
			for (int k = 0; k <= 200; k++)
			{
				for (int l = 0; l <= 200; l++)
				{
					_ = k == 0 && l == 0;
					if (AsteroidUnrotatedArray[k, l])
					{
						TotalAsteroidArea++;
					}
				}
			}
		}

		internal static void CreateAsteroidArray()
		{
			AsteroidArray = new bool[201, 201];
			int num = 0;
			int num2 = 0;
			double num3 = 0.0;
			double num4 = 0.0;
			double num5 = 0.0;
			double num6 = 0.0;
			for (int i = -100; i <= 100; i++)
			{
				num3 = (double)i * CosPA;
				num4 = (double)i * SinPA;
				for (int j = -100; j <= 100; j++)
				{
					num5 = (double)j * CosPA;
					num6 = (double)j * SinPA;
					num = Convert.ToInt32(num3 - num6);
					num2 = Convert.ToInt32(num4 + num5);
					if (num >= -100 && num <= 100 && num2 >= -100 && num2 <= 100)
					{
						AsteroidArray[i + CenterX, j + CenterY] = AsteroidUnrotatedArray[num + CenterX, num2 + CenterY];
					}
					else
					{
						AsteroidArray[i + CenterX, j + CenterY] = false;
					}
				}
			}
			if (!Utilities.IsInVisualStudio)
			{
				return;
			}
			int num7 = 0;
			for (int k = -100; k <= 100; k++)
			{
				for (int l = -100; l <= 100; l++)
				{
					if (GetAsteroid(k, l))
					{
						num7++;
					}
				}
			}
		}

		internal static float TotalBrightnessAtALocation(int AsteroidOffsetX, int AsteroidOffsetY)
		{
			int num = TotalStarIllumination;
			float num2 = BrightnessRatio_Asteroid_over_Star * (float)TotalStarIllumination;
			for (int i = -StarRadiusRange; i <= StarRadiusRange; i++)
			{
				int num3 = AsteroidOffsetX + i;
				if ((num3 < -AsteroidRadiusRange) | (num3 > AsteroidRadiusRange))
				{
					continue;
				}
				for (int j = -StarRadiusRange; j <= StarRadiusRange; j++)
				{
					int num4 = AsteroidOffsetY + j;
					if (!((num4 < -AsteroidRadiusRange) | (num4 > AsteroidRadiusRange)) && GetAsteroid(num3, num4))
					{
						num -= GetStar(i, j);
					}
				}
			}
			return ((float)num + num2) / ((float)TotalStarIllumination + num2);
		}

		internal void LightCurve(ref float[] OccBrightness, bool Along, int AcrossLocation)
		{
			string text;
			if (Along)
			{
				text = "FluxTestAlong.dat";
				OccBrightness = new float[201];
				for (int i = 0; i <= 200; i++)
				{
					OccBrightness[i] = TotalBrightnessAtALocation(i - 100, AcrossLocation);
				}
			}
			else
			{
				text = "FluxTestAcross.dat";
				OccBrightness = new float[201];
				float num = 0f;
				float num2 = 0f;
				float num3 = 0f;
				float num4 = 0f;
				int num5 = 0;
				for (int j = 0; j <= 200; j++)
				{
					num4 = (num3 = TotalBrightnessAtALocation(num5, j - 100));
					if ((SemiMajorAxis_mas != SemiMinorAxis_mas) | Rectangle)
					{
						if ((PA_MajorAxis % 90.0 != 0.0) | Rectangle)
						{
							num2 = TotalBrightnessAtALocation(num5 + 1, j - 100);
							num = TotalBrightnessAtALocation(num5 - 1, j - 100);
							if (num2 == num3 || num == num3)
							{
								OccBrightness[j] = num4;
							}
							else if (num2 < num4)
							{
								num3 = num2;
								do
								{
									num5++;
									num2 = TotalBrightnessAtALocation(num5, j - 100);
									if (Rectangle)
									{
										if (num2 > num3)
										{
											OccBrightness[j] = num3;
											break;
										}
										num3 = num2;
									}
									else
									{
										if (num2 >= num3)
										{
											OccBrightness[j] = num3;
											break;
										}
										num3 = num2;
									}
								}
								while (num5 <= 100);
							}
							else
							{
								if (!(num2 > num4))
								{
									continue;
								}
								num3 = num;
								do
								{
									num5--;
									num = TotalBrightnessAtALocation(num5, j - 100);
									if (Rectangle)
									{
										if (num > num3)
										{
											OccBrightness[j] = num;
											break;
										}
										num3 = num;
									}
									else
									{
										if (num >= num3)
										{
											OccBrightness[j] = num;
											break;
										}
										num3 = num;
									}
								}
								while (num5 >= -100);
							}
						}
						else
						{
							OccBrightness[j] = num4;
						}
					}
					else
					{
						OccBrightness[j] = num4;
					}
				}
			}
			if (!Utilities.RunningInDevelopmentEnvironment)
			{
				return;
			}
			using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "/Generated Files/" + text);
			for (int k = 0; k <= 200; k++)
			{
				streamWriter.WriteLine(string.Format("{0,1:f3}", OccBrightness[k]));
			}
		}
	}
}
