using System;
using System.Text;

namespace Occult.Asteroid_Observations
{
	internal class SatelliteData : IComparable
	{
		private double sat_dRA_mas;

		private double sat_dDec_mas;

		private double sat_d2RA_mas;

		private double sat_d2Dec_mas;

		private double satelliteSeparation;

		private double satellitePAapparent;

		private double satellitePA2000;

		private double draCosDecSat_atEvent;

		private double dDecSat_atEvent;

		private double sdev_draCosDecSat_atEvent;

		private double sdev_dDecSat_atEvent;

		private double sat_Sep_Uncertainty;

		private double sat_PA_Uncertainty;

		private double xSat_Geo_atEvent;

		private double ySat_Geo_atEvent;

		private double majorAxisSatellite = 10.0;

		private double minorAxisSatellite = 10.0;

		private double paAxisSatellite;

		private double dDecSat_atConj;

		private double draCosDecSat_atConj;

		private double day_Conj = 1.0;

		private double xSat_Geo_atConj;

		private double ySat_Geo_atConj;

		private double sep_Conj_star;

		private double pA_Conj_2000_star;

		private double sdev_T_Conj;

		private double sdev_Sep_Conj;

		private double sDev_AlongTrack;

		private int satelliteQuality;

		private int numberOfChords = 1;

		private int year_Conj = 2000;

		private int month_Conj = 1;

		private bool satelliteMotionIncluded;

		private string companionIAUname = "";

		private string cBET_Discovery = "";

		public string CompanionIAUname
		{
			get
			{
				return companionIAUname;
			}
			set
			{
				companionIAUname = value;
			}
		}

		public string CBET_Discovery
		{
			get
			{
				return cBET_Discovery;
			}
			set
			{
				cBET_Discovery = value;
			}
		}

		public double Sat_dRA_mas
		{
			get
			{
				return sat_dRA_mas;
			}
			set
			{
				sat_dRA_mas = value;
			}
		}

		public double Sat_dDec_mas
		{
			get
			{
				return sat_dDec_mas;
			}
			set
			{
				sat_dDec_mas = value;
			}
		}

		public double Sat_d2RA_mas
		{
			get
			{
				return sat_d2RA_mas;
			}
			set
			{
				sat_d2RA_mas = value;
			}
		}

		public double Sat_d2Dec_mas
		{
			get
			{
				return sat_d2Dec_mas;
			}
			set
			{
				sat_d2Dec_mas = value;
			}
		}

		public double SatelliteSeparation
		{
			get
			{
				return satelliteSeparation;
			}
			set
			{
				satelliteSeparation = value;
			}
		}

		public double SatellitePA_Apparent
		{
			get
			{
				return satellitePAapparent;
			}
			set
			{
				satellitePAapparent = value;
			}
		}

		public double SatellitePA_2000
		{
			get
			{
				return satellitePA2000;
			}
			set
			{
				satellitePA2000 = value;
			}
		}

		public double MajorAxisSatellite
		{
			get
			{
				return majorAxisSatellite;
			}
			set
			{
				majorAxisSatellite = value;
			}
		}

		public double MinorAxisSatellite
		{
			get
			{
				return minorAxisSatellite;
			}
			set
			{
				minorAxisSatellite = value;
			}
		}

		public double PAAxisSatellite
		{
			get
			{
				return paAxisSatellite;
			}
			set
			{
				paAxisSatellite = value;
			}
		}

		public int SatelliteQuality
		{
			get
			{
				return satelliteQuality;
			}
			set
			{
				satelliteQuality = value;
			}
		}

		public int NumberOfChords
		{
			get
			{
				return numberOfChords;
			}
			set
			{
				numberOfChords = value;
			}
		}

		public double XSat_Geo_atEvent
		{
			get
			{
				return xSat_Geo_atEvent;
			}
			set
			{
				xSat_Geo_atEvent = value;
			}
		}

		public double YSat_Geo_atEvent
		{
			get
			{
				return ySat_Geo_atEvent;
			}
			set
			{
				ySat_Geo_atEvent = value;
			}
		}

		public double dRACosDecSat_atEvent
		{
			get
			{
				return draCosDecSat_atEvent;
			}
			set
			{
				draCosDecSat_atEvent = value;
			}
		}

		public double ddecSat_atEvent
		{
			get
			{
				return dDecSat_atEvent;
			}
			set
			{
				dDecSat_atEvent = value;
			}
		}

		public double dRACosDecSat_atConj
		{
			get
			{
				return draCosDecSat_atConj;
			}
			set
			{
				draCosDecSat_atConj = value;
			}
		}

		public double ddecSat_atConj
		{
			get
			{
				return dDecSat_atConj;
			}
			set
			{
				dDecSat_atConj = value;
			}
		}

		public double Sat_Sep_Uncertainty
		{
			get
			{
				return sat_Sep_Uncertainty;
			}
			set
			{
				sat_Sep_Uncertainty = value;
			}
		}

		public double Sat_PA_Uncertainty
		{
			get
			{
				return sat_PA_Uncertainty;
			}
			set
			{
				sat_PA_Uncertainty = value;
			}
		}

		public double Sdev_dRACosDecSat_atEvent
		{
			get
			{
				return sdev_draCosDecSat_atEvent;
			}
			set
			{
				sdev_draCosDecSat_atEvent = value;
			}
		}

		public double Sdev_dDecSat_atEvent
		{
			get
			{
				return sdev_dDecSat_atEvent;
			}
			set
			{
				sdev_dDecSat_atEvent = value;
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

		public double XSat_Geo_atConj
		{
			get
			{
				return xSat_Geo_atConj;
			}
			set
			{
				xSat_Geo_atConj = value;
			}
		}

		public double YSat_Geo_atConj
		{
			get
			{
				return ySat_Geo_atConj;
			}
			set
			{
				ySat_Geo_atConj = value;
			}
		}

		public double Sep_Conj_Star
		{
			get
			{
				return sep_Conj_star;
			}
			set
			{
				sep_Conj_star = value;
			}
		}

		public double PA_Conj_2000_Star
		{
			get
			{
				return pA_Conj_2000_star;
			}
			set
			{
				pA_Conj_2000_star = value;
			}
		}

		public double Sdev_Sep_Conj
		{
			get
			{
				return sdev_Sep_Conj;
			}
			set
			{
				sdev_Sep_Conj = value;
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

		public double Sdev_AlongTrack
		{
			get
			{
				return sDev_AlongTrack;
			}
			set
			{
				sDev_AlongTrack = value;
			}
		}

		public bool SatelliteMotionIncluded
		{
			get
			{
				return satelliteMotionIncluded;
			}
			set
			{
				satelliteMotionIncluded = value;
			}
		}

		public int CompareTo(object other)
		{
			return -SatelliteSeparation.CompareTo(((SatelliteData)other).SatelliteSeparation);
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(CompanionIAUname.PadRight(16));
			stringBuilder.AppendFormat("{0,6:F1}", SatelliteSeparation);
			if ((Sat_Sep_Uncertainty > 0.0) & (Sat_Sep_Uncertainty < 10.0))
			{
				stringBuilder.AppendFormat(" ±{0,4:F1}mas", Sat_Sep_Uncertainty);
			}
			else
			{
				stringBuilder.Append("mas".PadRight(9));
			}
			stringBuilder.AppendFormat(", {0,6:F2}°", SatellitePA_Apparent);
			if ((Sat_PA_Uncertainty > 0.0) & (Sat_PA_Uncertainty < 50.0))
			{
				stringBuilder.AppendFormat(" ±{0,4:F2}°", Sat_PA_Uncertainty);
			}
			else
			{
				stringBuilder.Append("".PadRight(9));
			}
			return stringBuilder.ToString();
		}
	}
}
