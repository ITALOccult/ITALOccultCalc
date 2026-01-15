using System;

namespace Occult.Asteroid_Observations
{
	public class MPCName : IComparable
	{
		private string observerID;

		private string eventID;

		private string siteLongitude = "";

		private string siteLatitude = "";

		private string siteAlt = "";

		private string telescopeAperture = "";

		private int region = -1;

		public string ObserverID
		{
			get
			{
				return observerID;
			}
			set
			{
				observerID = value;
			}
		}

		public string EventID
		{
			get
			{
				return eventID;
			}
			set
			{
				eventID = value;
			}
		}

		public string SiteLongitude
		{
			get
			{
				return siteLongitude;
			}
			set
			{
				siteLongitude = value;
			}
		}

		public string SiteLatitude
		{
			get
			{
				return siteLatitude;
			}
			set
			{
				siteLatitude = value;
			}
		}

		public string SiteAlt
		{
			get
			{
				return siteAlt;
			}
			set
			{
				siteAlt = value;
			}
		}

		public string TelescopeAperture
		{
			get
			{
				return telescopeAperture;
			}
			set
			{
				telescopeAperture = value;
			}
		}

		public string ObserverDetails => ObserverID.PadRight(16) + SiteLongitude.PadRight(14) + SiteLatitude.PadRight(12) + SiteAlt.PadRight(6) + TelescopeAperture.PadRight(6);

		public int Region
		{
			get
			{
				return region;
			}
			set
			{
				region = value;
			}
		}

		public int CompareTo(object other)
		{
			int num = ObserverID.IndexOf(" ") + 1;
			if (num < 0)
			{
				num = 0;
			}
			int num2 = ((MPCName)other).ObserverID.IndexOf(" ") + 1;
			if (num2 < 0)
			{
				num2 = 0;
			}
			if (ObserverID.Substring(num).ToUpper() == ((MPCName)other).ObserverID.Substring(num2).ToUpper())
			{
				return ObserverID.ToUpper().CompareTo(((MPCName)other).ObserverID.ToUpper());
			}
			return ObserverID.Substring(num).ToUpper().CompareTo(((MPCName)other).ObserverID.Substring(num2).ToUpper());
		}

		public override string ToString()
		{
			return ObserverID;
		}
	}
}
