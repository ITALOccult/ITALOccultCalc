using System;

namespace Occult.Asteroid_Observations
{
	internal class SatellitePositions
	{
		private string IAU_satelliteID = "";

		private double sepOfSatellite;

		private double pA_SatConj_Star2000;

		private double dRAfromStar_Satellite;

		private double dDecfromStar_Satellite;

		private double sdev_dRAfromStar_Satellite;

		private double sdev_dDecfromStar_Satellite;

		private double day_Conj;

		private double sdev_Sep_fromStar_Satellite;

		private double sdev_T_Conj;

		private double sdev_AlongTrack;

		private double fPlane_X;

		private double fPlane_Y;

		private double equatorial_X;

		private double equatorial_Y;

		private double equatorial_Z;

		private string xyz_DecimalPlaces;

		private int num_ChordsSatellite;

		private int satelliteFitQuality;

		private int year_Conj;

		private int month_Conj;

		public string IAUSatelliteID
		{
			get
			{
				if (IAU_satelliteID.Trim() == "")
				{
					return "?";
				}
				return IAU_satelliteID;
			}
			set
			{
				IAU_satelliteID = value;
			}
		}

		public double FPlane_X
		{
			get
			{
				return fPlane_X;
			}
			set
			{
				fPlane_X = value;
			}
		}

		public double FPlane_Y
		{
			get
			{
				return fPlane_Y;
			}
			set
			{
				fPlane_Y = value;
			}
		}

		public double Equatorial_X
		{
			get
			{
				return equatorial_X;
			}
			set
			{
				equatorial_X = value;
			}
		}

		public double Equatorial_Y
		{
			get
			{
				return equatorial_Y;
			}
			set
			{
				equatorial_Y = value;
			}
		}

		public double Equatorial_Z
		{
			get
			{
				return equatorial_Z;
			}
			set
			{
				equatorial_Z = value;
			}
		}

		public double Sep_Conj_Star
		{
			get
			{
				return sepOfSatellite;
			}
			set
			{
				sepOfSatellite = value;
			}
		}

		public double PA_Conj_Star2000
		{
			get
			{
				return pA_SatConj_Star2000;
			}
			set
			{
				pA_SatConj_Star2000 = value;
			}
		}

		public double PAofTrack_2000 => Utilities.Normalise_0to180(PA_Conj_Star2000 + 90.0);

		public double dRA_fromStar_Satellite
		{
			get
			{
				return dRAfromStar_Satellite;
			}
			set
			{
				dRAfromStar_Satellite = value;
			}
		}

		public double dDec_fromStar_Satellite
		{
			get
			{
				return dDecfromStar_Satellite;
			}
			set
			{
				dDecfromStar_Satellite = value;
			}
		}

		public double Sep_fromStar_Satellite => Math.Sqrt(Math.Pow(dRA_fromStar_Satellite, 2.0) + Math.Pow(dDec_fromStar_Satellite, 2.0));

		public double PA_fromStar_Satellite
		{
			get
			{
				double num = Math.Atan2(dRA_fromStar_Satellite, dDec_fromStar_Satellite) * (180.0 / Math.PI);
				if (num < 0.0)
				{
					num += 360.0;
				}
				return num;
			}
		}

		public double Sdev_dRA_fromStar_Satellite
		{
			get
			{
				return sdev_dRAfromStar_Satellite;
			}
			set
			{
				sdev_dRAfromStar_Satellite = value;
			}
		}

		public double Sdev_dDec_fromStar_Satellite
		{
			get
			{
				return sdev_dDecfromStar_Satellite;
			}
			set
			{
				sdev_dDecfromStar_Satellite = value;
			}
		}

		public double AcrossTrack_fromFit_Satellite
		{
			get
			{
				return sdev_Sep_fromStar_Satellite;
			}
			set
			{
				sdev_Sep_fromStar_Satellite = value;
			}
		}

		public int Number_ChordsSatellite
		{
			get
			{
				return num_ChordsSatellite;
			}
			set
			{
				num_ChordsSatellite = value;
			}
		}

		public int SatelliteFitQuality
		{
			get
			{
				return satelliteFitQuality;
			}
			set
			{
				satelliteFitQuality = value;
			}
		}

		public int Year_Conj
		{
			get
			{
				return year_Conj;
			}
			set
			{
				year_Conj = value;
			}
		}

		public int Month_Conj
		{
			get
			{
				return month_Conj;
			}
			set
			{
				month_Conj = value;
			}
		}

		public double Day_Conj
		{
			get
			{
				return day_Conj;
			}
			set
			{
				day_Conj = value;
			}
		}

		public double Sdev_T_Conj
		{
			get
			{
				return sdev_T_Conj;
			}
			set
			{
				sdev_T_Conj = value;
			}
		}

		public double Sdev_AlongTrack_fromFit
		{
			get
			{
				return sdev_AlongTrack;
			}
			set
			{
				sdev_AlongTrack = value;
			}
		}
	}
}
