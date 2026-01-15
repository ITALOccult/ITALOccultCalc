using System;
using System.Text;
using Occult.Properties;

namespace Occult
{
	public class Sites : IComparable
	{
		public static int SortField;

		private double longitude;

		private double latitude;

		private double altitude;

		private double geoidHeight;

		private double psin;

		private double pcos;

		private double grazeTravelDist = 100.0;

		private float magCorrection;

		private float apertureCM = 10f;

		private float timeZone;

		private int plotOnMap;

		private int plotInGoogle;

		private string name = "";

		private string shortName = "";

		private const double Radian = 180.0 / Math.PI;

		public double Longitude
		{
			get
			{
				return longitude;
			}
			set
			{
				longitude = value;
			}
		}

		public double LongitudeApprox => Math.Round(longitude * 30.0) / 30.0;

		public double Latitude
		{
			get
			{
				return latitude;
			}
			set
			{
				latitude = value;
				Geocentric();
			}
		}

		public double LatitudeApprox => Math.Round(latitude * 30.0) / 30.0;

		public double Altitude
		{
			get
			{
				return altitude;
			}
			set
			{
				altitude = value;
				Geocentric();
			}
		}

		public double GeoidHeight
		{
			get
			{
				return geoidHeight;
			}
			set
			{
				geoidHeight = value;
			}
		}

		public double GrazeTravelDist
		{
			get
			{
				return grazeTravelDist;
			}
			set
			{
				grazeTravelDist = value;
			}
		}

		public double pSin => psin;

		public double pCos => pcos;

		public float TimeZone
		{
			get
			{
				return timeZone;
			}
			set
			{
				timeZone = value;
			}
		}

		public float ApertureCM
		{
			get
			{
				return apertureCM;
			}
			set
			{
				apertureCM = value;
			}
		}

		public float MagCorrection
		{
			get
			{
				return magCorrection;
			}
			set
			{
				magCorrection = value;
			}
		}

		public int PlotOnMap
		{
			get
			{
				return plotOnMap;
			}
			set
			{
				plotOnMap = value;
			}
		}

		public int PlotInGoogle
		{
			get
			{
				return plotInGoogle;
			}
			set
			{
				plotInGoogle = value;
			}
		}

		public string Name
		{
			get
			{
				return name;
			}
			set
			{
				name = value;
				if (this.NameChanged != null)
				{
					this.NameChanged(this, EventArgs.Empty);
				}
			}
		}

		public string ShortName
		{
			get
			{
				return shortName;
			}
			set
			{
				shortName = value;
			}
		}

		public event EventHandler NameChanged;

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("{0,11:F6}", longitude);
			stringBuilder.AppendFormat("{0,11:F6}", latitude);
			stringBuilder.AppendFormat("{0,5:F0}", altitude);
			stringBuilder.AppendFormat("{0,5:F0}", apertureCM);
			stringBuilder.AppendFormat("{0,5:F1}", magCorrection);
			stringBuilder.AppendFormat("{0,5:F0} ", grazeTravelDist);
			stringBuilder.Append(name.PadRight(32).Substring(0, 32));
			stringBuilder.AppendFormat("{0,2:F0} ", plotOnMap);
			stringBuilder.Append(shortName.PadRight(9).Substring(0, 9));
			stringBuilder.AppendFormat("{0,6:F1}", timeZone);
			stringBuilder.AppendFormat("{0,2:F0}", plotInGoogle);
			return stringBuilder.ToString();
		}

		public int CompareTo(object other)
		{
			if (SortField == 0)
			{
				return name.CompareTo(((Sites)other).Name);
			}
			if (SortField == 1)
			{
				return longitude.CompareTo(((Sites)other).Longitude);
			}
			return ((Sites)other).Latitude.CompareTo(latitude);
		}

		internal void Read_SiteFile(string InLine)
		{
			InLine = InLine.PadRight(95);
			if (!double.TryParse(InLine.Substring(0, 11), out longitude))
			{
				longitude = 0.0;
			}
			if (longitude > 180.0)
			{
				Longitude -= 360.0;
			}
			if (longitude < -180.0)
			{
				longitude += 360.0;
			}
			if (!double.TryParse(InLine.Substring(11, 11), out latitude))
			{
				latitude = 0.0;
			}
			if (!double.TryParse(InLine.Substring(22, 5), out altitude))
			{
				altitude = 0.0;
			}
			if (!float.TryParse(InLine.Substring(27, 5), out apertureCM))
			{
				apertureCM = (float)Settings.Default.Site_TelescopeAperture_cm;
			}
			if (!float.TryParse(InLine.Substring(32, 5), out magCorrection))
			{
				magCorrection = (float)Settings.Default.Site_MagnitudeCorrection;
			}
			if (!double.TryParse(InLine.Substring(37, 5), out grazeTravelDist))
			{
				grazeTravelDist = (float)Settings.Default.Site_GrazeTravelDisance;
			}
			name = InLine.Substring(43, 32).Trim();
			if (!int.TryParse(InLine.Substring(75, 2), out plotOnMap))
			{
				plotOnMap = 0;
			}
			shortName = InLine.Substring(78, 9).Trim();
			if ((shortName.Length < 1) & (plotOnMap > 0))
			{
				shortName = name.PadRight(9).Substring(0, 9).Trim();
			}
			if (!float.TryParse(InLine.Substring(87, 6), out timeZone))
			{
				timeZone = (float)Math.Round(longitude / 15.0);
			}
			if (!int.TryParse(InLine.Substring(93, 2), out plotInGoogle))
			{
				plotInGoogle = 0;
			}
			geoidHeight = Utilities.GeoidHeight(longitude, latitude);
			double ObserverLongitude = 0.0;
			Utilities.GetGeodetic(Latitude / (180.0 / Math.PI), altitude, Altitude_is_MSL: true, out pcos, out psin, 0, ref ObserverLongitude);
			Geocentric();
		}

		private void Geocentric()
		{
			double num = latitude / (180.0 / Math.PI);
			double num2 = 1.0 / Math.Sqrt(Math.Cos(num) * Math.Cos(num) + 0.993305615000412 * Math.Sin(num) * Math.Sin(num));
			double num3 = 0.993305615000412 * num2;
			psin = (num3 + (altitude + geoidHeight) / 6378137.0) * Math.Sin(num);
			pcos = (num2 + (altitude + geoidHeight) / 6378137.0) * Math.Cos(num);
		}

		internal void Read_Old_SiteFile(string NameLine1, string LongitudeLine2, string LatitudeLine3, string AltitudeLine4, string ApertureLine6, string MagCorrnLine7)
		{
			name = NameLine1.Trim();
			longitude = double.Parse(LongitudeLine2.Substring(1, 4)) + double.Parse(LongitudeLine2.Substring(4, 3)) / 60.0 + double.Parse(LongitudeLine2.Substring(7)) / 3600.0;
			if (LongitudeLine2.Substring(0, 1) == "-")
			{
				longitude = 0.0 - longitude;
			}
			latitude = double.Parse(LatitudeLine3.Substring(1, 3)) + double.Parse(LatitudeLine3.Substring(4, 3)) / 60.0 + double.Parse(LatitudeLine3.Substring(7)) / 3600.0;
			if (LatitudeLine3.Substring(0, 1) == "-")
			{
				latitude = 0.0 - latitude;
			}
			altitude = double.Parse(AltitudeLine4);
			apertureCM = int.Parse(ApertureLine6) / 10;
			magCorrection = float.Parse(MagCorrnLine7);
			grazeTravelDist = (double)Settings.Default.Site_GrazeTravelDisance;
			shortName = name.PadRight(9).Substring(0, 9).Trim();
			plotOnMap = Settings.Default.Site_PlotControl;
			timeZone = (float)Math.Round(longitude / 15.0);
		}

		internal void Read_Old_MultiSiteFile(string InLine)
		{
			name = InLine.Substring(0, 16).Trim();
			if (!double.TryParse(InLine.Substring(16, 4).Replace("-", " ").Replace("+", " "), out var result))
			{
				result = 0.0;
			}
			if (!double.TryParse(InLine.Substring(21, 4), out var result2))
			{
				result2 = 0.0;
			}
			longitude = Math.Abs(result) + result2 / 60.0;
			if (InLine.Substring(16, 4).IndexOf("-") > -1)
			{
				longitude = 0.0 - longitude;
			}
			if (!double.TryParse(InLine.Substring(26, 3).Replace("-", " ").Replace("+", " "), out result))
			{
				result = 0.0;
			}
			if (!double.TryParse(InLine.Substring(30, 4), out result2))
			{
				result2 = 0.0;
			}
			latitude = Math.Abs(result) + result2 / 60.0;
			if (InLine.Substring(26, 3).IndexOf("-") > -1)
			{
				latitude = 0.0 - latitude;
			}
			if (!double.TryParse(InLine.Substring(35, 5), out altitude))
			{
				altitude = 0.0;
			}
			apertureCM = (float)Settings.Default.Site_TelescopeAperture_cm;
			magCorrection = (float)Settings.Default.Site_MagnitudeCorrection;
			grazeTravelDist = (double)Settings.Default.Site_GrazeTravelDisance;
			shortName = name.PadRight(9).Substring(0, 9).Trim();
			plotOnMap = Settings.Default.Site_PlotControl;
			timeZone = (float)Math.Round(longitude / 15.0);
		}

		internal void Read_Old_MultiLocationFile(string InLine)
		{
			if (InLine.Length < 56)
			{
				InLine = InLine.PadRight(56);
			}
			name = InLine.Substring(0, 32).Trim();
			if (!double.TryParse(InLine.Substring(32, 4).Replace("-", " ").Replace("+", " "), out var result))
			{
				result = 0.0;
			}
			if (!double.TryParse(InLine.Substring(37, 4), out var result2))
			{
				result2 = 0.0;
			}
			longitude = Math.Abs(result) + result2 / 60.0;
			if (InLine.Substring(32, 4).IndexOf("-") > -1)
			{
				longitude = 0.0 - longitude;
			}
			if (!double.TryParse(InLine.Substring(42, 3).Replace("-", " ").Replace("+", " "), out result))
			{
				result = 0.0;
			}
			if (!double.TryParse(InLine.Substring(46, 4), out result2))
			{
				result2 = 0.0;
			}
			latitude = Math.Abs(result) + result2 / 60.0;
			if (InLine.Substring(42, 3).IndexOf("-") > -1)
			{
				latitude = 0.0 - latitude;
			}
			if (!double.TryParse(InLine.Substring(51, 5), out altitude))
			{
				altitude = 0.0;
			}
			apertureCM = (float)Settings.Default.Site_TelescopeAperture_cm;
			magCorrection = (float)Settings.Default.Site_MagnitudeCorrection;
			grazeTravelDist = (double)Settings.Default.Site_GrazeTravelDisance;
			shortName = name.PadRight(9).Substring(0, 9).Trim();
			plotOnMap = Settings.Default.Site_PlotControl;
			timeZone = (float)Math.Round(longitude / 15.0);
		}

		internal void Read_Old_CitiesFile(string InLine)
		{
			int num = InLine.IndexOf(",");
			int num2 = InLine.IndexOf(",", num + 1);
			if (!double.TryParse(InLine.Substring(0, num), out longitude))
			{
				longitude = 0.0;
			}
			if (!double.TryParse(InLine.Substring(num + 1, num2 - num - 1), out latitude))
			{
				latitude = 0.0;
			}
			name = InLine.Substring(num2 + 1).Trim();
			altitude = 0.0;
			apertureCM = (float)Settings.Default.Site_TelescopeAperture_cm;
			magCorrection = (float)Settings.Default.Site_MagnitudeCorrection;
			grazeTravelDist = (double)Settings.Default.Site_GrazeTravelDisance;
			shortName = name.PadRight(9).Substring(0, 9).Trim();
			plotOnMap = Settings.Default.Site_PlotControl;
			timeZone = (float)Math.Round(longitude / 15.0);
		}
	}
}
